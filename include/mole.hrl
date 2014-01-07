
-define(APP, mole).

-define(SERVER_REQ, 1).
-define(SERVER_RES, 2).
-define(WAN_CONN, 3).
-define(LAN_CONN, 4).
-define(BCAST_CONN, 5).
-define(P2P_HEARTBEAT, 6).

-define(ETS_MOLE, ets_mole).

-record(key, {my_key, his_key}).
