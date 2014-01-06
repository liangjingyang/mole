




## Protocol

    SERVER_REQ = 1
    SERVER_RES = 2
    WAN_CONN = 3
    LAN_CONN = 4
    P2P_DATA = 5


#### client to server

    1 byte:     SERVER_REQ
    16 byte:    my_key
    16 byte:    his_key 
    N byte:     my_packet

#### server to client

    1 byte:     SERVER_RES 
    16 byte:    his_key
    4 byte:     his_wan_ip
    2 byte:     his_wan_port
    N byte:     his_packet

#### p2p connect

    1 byte:     P2P_CONN

#### p2p data

    1 byte:     P2P_DATA
    N byte:     data



