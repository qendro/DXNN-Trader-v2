
-module(scape).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,Name} ->
			scape:Name(ExoSelf_PId)
	end.
	
fx_sim(Exoself_PId)->
	fx:sim(Exoself_PId).

%% Live bridge: start/link live_scape and signal live simulation
live_sim(Exoself_PId) ->
    case whereis(live_scape) of
        undefined ->
            case live_scape:start_link() of
                {ok, _Pid} -> ok;
                _ -> ok
            end;
        _Pid -> ok
    end,
    fx:sim(Exoself_PId).
    %live_scape ! {Exoself_PId, live_sim}.
