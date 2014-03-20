{application,erl2048,
             [{description,"2048 game server."},
              {vsn,"1"},
              {modules,[erl2048_app,erl2048_sup,game,grid,starter,ws_handler]},
              {registered,[erl2048_sup]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{erl2048_app,[]}},
              {env,[]}]}.
