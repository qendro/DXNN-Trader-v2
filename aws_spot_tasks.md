# AWS Spot Instance Interruption Handling - Simple Implementation

## Overview
This document outlines the minimal implementation for AWS spot instance interruption handling using systemd + bash watcher approach. This approach requires minimal Erlang code changes and leverages existing infrastructure.

## Why This Approach is Better

### ✅ **Minimal Code Changes**
- Only **one function** added to `benchmarker.erl`
- **Zero new Erlang modules** 
- **No HTTP dependencies** in the BEAM
- **No complex state management**

### ✅ **Leverages Existing Infrastructure**
- Uses **Mnesia backup/restore** (already your persistence layer)
- **Systemd** handles service management
- **Bash** handles IMDSv2 (simpler than Erlang HTTP)
- **AWS CLI** for S3 uploads (no Erlang S3 libraries needed)

### ✅ **Production Ready**
- **IMDSv2 compliant** with proper token handling
- **Jitter and backoff** for polling
- **Systemd service** with restart capabilities
- **S3 integration** via ExecStopPost

### ✅ **Testable**
- Can test with **EC2 Metadata Mock** locally
- **No need to kill real spot instances** for testing
- **Auditable** - simple bash script and one Erlang function

## Task 1: Add Checkpoint Function to Benchmarker
**File**: `benchmarker.erl`

- [ ] Add `checkpoint_and_exit/0` to exports
- [ ] Implement `checkpoint_and_exit/0` function
- [ ] Add pause call to population_monitor
- [ ] Add Mnesia sync and backup operations
- [ ] Add graceful shutdown with init:stop()

```erlang
-export([checkpoint_and_exit/0]).

checkpoint_and_exit() ->
    %% Stop scheduling new work (best-effort, fast)
    catch gen_server:call(population_monitor, pause, 1000),
    %% Flush Mnesia to disk & make a point-in-time backup
    ok = mnesia:sync_log(),
    {ok, _} = filelib:ensure_dir("/var/lib/dxnn/"),
    Backup = "/var/lib/dxnn/backup-" ++ integer_to_list(erlang:system_time(second)) ++ ".dmp",
    ok = mnesia:backup(Backup),
    %% Optional: upload in background via systemd ExecStopPost or cron
    init:stop().
```

## Task 2: Add Restore Function to Benchmarker
**File**: `benchmarker.erl`

- [ ] Add `maybe_restore/0` to exports
- [ ] Implement `maybe_restore/0` function
- [ ] Add file pattern matching for backup files
- [ ] Add Mnesia restore operation
- [ ] Handle case when no backups exist

```erlang
-export([maybe_restore/0]).

maybe_restore() ->
    case filelib:wildcard("/var/lib/dxnn/backup-*.dmp") of
        [] -> ok;
        Files ->
            Latest = lists:last(lists:sort(Files)),
            {atomic, _} = mnesia:restore(Latest, [{default_op, recreate_tables}]),
            ok
    end.
```

## Task 3: Manual Restore Process
**User Action Required**

- [ ] Document manual restore procedure
- [ ] Add restore instructions to README
- [ ] Create restore script or command examples

### Manual Restore Instructions
After a spot instance interruption and restart, the user must manually call:

```erlang
benchmarker:maybe_restore().
```

This will:
- Check for backup files in `/var/lib/dxnn/`
- Restore the most recent backup if found
- Continue training from the checkpoint
- Do nothing if no backups exist

### Example Usage
```bash
# Connect to running Erlang node and call restore
erl -noshell -name ctl@127.0.0.1 -setcookie YOURCOOKIE \
    -eval 'rpc:call(benchmarker, maybe_restore, [], 5000), halt().'
```

## Task 4: Create Bash Watcher Script
**File**: `/usr/local/bin/spot-watch.sh`

- [ ] Create the bash script with IMDSv2 support
- [ ] Implement token-based authentication
- [ ] Add polling for rebalance and interruption notices
- [ ] Add jitter and error handling
- [ ] Add RPC call to Erlang checkpoint function
- [ ] Add logging to `/var/log/spot-watch.log`

```bash
#!/usr/bin/env bash
set -euo pipefail
IMDS="http://169.254.169.254"
tok() { curl -sS -X PUT "$IMDS/latest/api/token" -H "X-aws-ec2-metadata-token-ttl-seconds: 21600"; }
get() { curl -sS -H "X-aws-ec2-metadata-token: $1" "$IMDS$2" -f || return 1; }

TOKEN=""
touch /var/log/spot-watch.log

while true; do
  [ -z "${TOKEN}" ] && TOKEN="$(tok || true)"
  sleep $(( (RANDOM % 5) + 3 ))  # jitter 3–7s

  # Rebalance (early signal)
  if get "${TOKEN}" "/latest/meta-data/events/recommendations/rebalance" > /dev/null 2>&1; then
    echo "$(date -Is) rebalance detected" >> /var/log/spot-watch.log
    # optional: preemptively checkpoint (non-fatal)
    erl -noshell -name ctl@127.0.0.1 -setcookie YOURCOOKIE \
        -eval 'rpc:call(benchmarker, checkpoint_and_exit, [], 5000), halt().' || true
    exit 0
  fi

  # 2-minute interruption
  if get "${TOKEN}" "/latest/meta-data/spot/instance-action" > /dev/null 2>&1; then
    echo "$(date -Is) interruption detected" >> /var/log/spot-watch.log
    erl -noshell -name ctl@127.0.0.1 -setcookie YOURCOOKIE \
        -eval 'rpc:call(benchmarker, checkpoint_and_exit, [], 5000), halt().' || true
    exit 0
  fi
done
```

## Task 5: Create Systemd Service
**File**: `/etc/systemd/system/spot-watch.service`

- [ ] Create systemd service file
- [ ] Configure service dependencies
- [ ] Add restart policy
- [ ] Add ExecStopPost for S3 upload (optional)
- [ ] Configure proper user and permissions

```ini
[Unit]
Description=AWS Spot interruption watcher
After=network-online.target

[Service]
Type=simple
ExecStart=/usr/local/bin/spot-watch.sh
Restart=always
RestartSec=5

# Optional: mirror latest backup to S3 after we exit gracefully
ExecStopPost=/bin/bash -lc 'aws s3 cp $(ls -1 /var/lib/dxnn/backup-*.dmp | tail -n1) s3://YOUR-BUCKET/dxnn/ || true'

[Install]
WantedBy=multi-user.target
```

## Task 6: Create Backup Directory
**System Command**

- [ ] Create backup directory: `mkdir -p /var/lib/dxnn/`
- [ ] Set proper permissions: `chmod 755 /var/lib/dxnn/`
- [ ] Ensure Erlang process can write to directory
- [ ] Test write access

## Task 7: Configure AWS IAM Role (Optional)
**AWS Configuration**

- [ ] Create IAM role for EC2 instance
- [ ] Add S3 permissions for backup bucket
- [ ] Attach role to EC2 instance
- [ ] Test S3 access from instance

## Task 8: Install and Enable Service
**System Commands**

- [ ] Make script executable: `chmod +x /usr/local/bin/spot-watch.sh`
- [ ] Reload systemd: `sudo systemctl daemon-reload`
- [ ] Enable service: `sudo systemctl enable spot-watch`
- [ ] Start service: `sudo systemctl start spot-watch`
- [ ] Check service status: `sudo systemctl status spot-watch`

## Task 9: Update Erlang Cookie Configuration
**Configuration**

- [ ] Set Erlang cookie in environment or config
- [ ] Update bash script with correct cookie value
- [ ] Ensure RPC calls work between processes
- [ ] Test RPC connectivity

## Task 10: Testing and Validation

### Task 10.1: Local Testing
- [ ] Test `benchmarker:checkpoint_and_exit/0` manually
- [ ] Verify backup file creation
- [ ] Test `benchmarker:maybe_restore/0` manually
- [ ] Verify restore functionality

### Task 10.2: Service Testing
- [ ] Check service is running: `systemctl status spot-watch`
- [ ] Check logs: `tail -f /var/log/spot-watch.log`
- [ ] Verify polling is working
- [ ] Test service restart: `systemctl restart spot-watch`

### Task 10.3: AWS Testing
- [ ] Use EC2 Metadata Mock to simulate interruptions
- [ ] Test rebalance detection
- [ ] Test interruption detection
- [ ] Verify graceful shutdown and backup creation

### Task 10.4: Recovery Testing
- [ ] Create backup on one instance
- [ ] Launch new instance with same EBS volume
- [ ] Verify manual restore works correctly
- [ ] Test restore command from user perspective
- [ ] Confirm training resumes from checkpoint after manual restore

## Task 11: S3 Integration (Optional)

### Task 11.1: Configure S3 Bucket
- [ ] Create S3 bucket for backups
- [ ] Set bucket permissions
- [ ] Configure lifecycle policies
- [ ] Test S3 access

### Task 11.2: Update Systemd Service
- [ ] Add AWS CLI installation
- [ ] Configure AWS credentials
- [ ] Test S3 upload in ExecStopPost
- [ ] Verify backup files in S3

## Task 12: Monitoring and Logging

### Task 12.1: Add Logging
- [ ] Add logging to `checkpoint_and_exit/0`
- [ ] Add logging to `maybe_restore/0`
- [ ] Configure log rotation for spot-watch.log
- [ ] Add monitoring for service health

### Task 12.2: Add Metrics
- [ ] Track checkpoint frequency
- [ ] Monitor backup file sizes
- [ ] Track restore success rate
- [ ] Add CloudWatch metrics (optional)

## Task 13: Documentation

- [ ] Update README.md with spot instance handling
- [ ] Document configuration options
- [ ] Add troubleshooting guide
- [ ] Document testing procedures
- [ ] Add deployment notes

## Task 14: Production Deployment

### Task 14.1: AMI Preparation
- [ ] Include spot-watch script in AMI
- [ ] Include systemd service in AMI
- [ ] Configure backup directory in AMI
- [ ] Test AMI functionality

### Task 14.2: Launch Configuration
- [ ] Update user data scripts
- [ ] Configure IAM roles
- [ ] Set up S3 bucket access
- [ ] Test launch process

## Task 15: Validation Checklist

- [ ] Service starts automatically on boot
- [ ] Polling detects rebalance recommendations
- [ ] Polling detects termination notices
- [ ] Checkpoint function creates backup files
- [ ] Restore function recovers from backups
- [ ] S3 upload works (if configured)
- [ ] Service restarts on failure
- [ ] Logs are properly rotated
- [ ] No performance impact on training
- [ ] Graceful shutdown under all conditions

## Configuration Variables

### Required Configuration
- `YOURCOOKIE` - Erlang cookie for RPC calls
- `YOUR-BUCKET` - S3 bucket name for backups (optional)

### Optional Configuration
- Backup directory: `/var/lib/dxnn/` (default)
- Log file: `/var/log/spot-watch.log` (default)
- Polling interval: 3-7 seconds with jitter (default)
- Token TTL: 6 hours (default)

## Files to Create/Modify

### New Files
- `/usr/local/bin/spot-watch.sh` - Bash watcher script
- `/etc/systemd/system/spot-watch.service` - Systemd service

### Modified Files
- `benchmarker.erl` - Add checkpoint_and_exit/0 and maybe_restore/0

### Directories
- `/var/lib/dxnn/` - Backup storage directory

## Progress Tracking

**Overall Progress**: [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] (0/10)

**Completed Tasks**: 0/15 major tasks

**Next Priority**: Task 1 - Add Checkpoint Function to Benchmarker

## Notes

- All bash scripts should be tested on actual EC2 instances
- Use EC2 Metadata Mock for local testing
- Monitor system resources during checkpoint operations
- Keep backup files organized and implement cleanup policies
- Test the complete flow before production deployment
