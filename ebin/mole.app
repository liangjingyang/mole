{application,mole,
             [{description,"udp make hole"},
              {vsn,"0.0.1"},
              {modules,[mole,mole_client,mole_server,mole_sup,mole_worker]},
              {registered,[mole_sup,mole_server]},
              {applications,[kernel,stdlib]},
              {mod,{mole,[]}},
              {env,[{worker_num,10}]}]}.
