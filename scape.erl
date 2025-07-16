%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

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



distance(Vector1,Vector2)->
	distance(Vector1,Vector2,0).	
distance([Val1|Vector1],[Val2|Vector2],Acc)->
	distance(Vector1,Vector2,Acc+math:pow(Val2-Val1,2));
distance([],[],Acc)->
	math:sqrt(Acc).
	
fx_sim(Exoself_PId)->
	fx:sim(Exoself_PId).
