with Ada.Text_IO;

package body Drivers.Ethernet is

   procedure Initialize (This : in out Ethernet_Type) is

      Server_Address : GNAT.Sockets.Sock_Addr_Type;

   begin

      -- set the server address
      Server_Address :=
        GNAT.Sockets.Sock_Addr_Type'
          (Family => GNAT.Sockets.Family_Inet,
           Addr   =>
             GNAT.Sockets.Inet_Addr_Type'
               (Family => GNAT.Sockets.Family_Inet,
                Sin_V4 =>
                  GNAT.Sockets.Inet_Addr_V4_Type'
                    (1 =>
                       GNAT.Sockets.Inet_Addr_Comp_Type (This.Address.all (1)),
                     2 =>
                       GNAT.Sockets.Inet_Addr_Comp_Type (This.Address.all (2)),
                     3 =>
                       GNAT.Sockets.Inet_Addr_Comp_Type (This.Address.all (3)),
                     4 =>
                       GNAT.Sockets.Inet_Addr_Comp_Type
                         (This.Address.all (4)))),
           Port   => GNAT.Sockets.Port_Type (This.Port));

      -- print the server address
      Ada.Text_IO.Put_Line
        ("IP" & This.Address (1)'Image & "." & This.Address (2)'Image & "." &
         This.Address (3)'Image & "." & This.Address (4)'Image & ":" &
         This.Port'Image);

      -- create the socket
      GNAT.Sockets.Create_Socket
        (This.Socket, GNAT.Sockets.Family_Inet, GNAT.Sockets.Socket_Datagram);

      -- set multicast loopback
      GNAT.Sockets.Set_Socket_Option
        (This.Socket, GNAT.Sockets.IP_Protocol_For_IP_Level,
         (GNAT.Sockets.Multicast_Loop, False));

      -- enable reuse address
      GNAT.Sockets.Set_Socket_Option
        (This.Socket, GNAT.Sockets.Socket_Level,
         (GNAT.Sockets.Reuse_Address, True));

      -- create a selector
      GNAT.Sockets.Create_Selector (This.Selector);

      -- bind the socket to the address
      GNAT.Sockets.Bind_Socket (This.Socket, Server_Address);

   end Initialize;

   procedure Join_Multicast_Group
     (This : in out Ethernet_Type; Address : Address_V4_Type)
   is

      Multicast_Address : GNAT.Sockets.Inet_Addr_Type;

   begin

      -- set the multicast address
      Multicast_Address :=
        GNAT.Sockets.Inet_Addr_Type'
          (Family => GNAT.Sockets.Family_Inet,
           Sin_V4 =>
             GNAT.Sockets.Inet_Addr_V4_Type'
               (1 => GNAT.Sockets.Inet_Addr_Comp_Type (Address (1)),
                2 => GNAT.Sockets.Inet_Addr_Comp_Type (Address (2)),
                3 => GNAT.Sockets.Inet_Addr_Comp_Type (Address (3)),
                4 => GNAT.Sockets.Inet_Addr_Comp_Type (Address (4))));

      -- join the multicast group
      GNAT.Sockets.Set_Socket_Option
        (This.Socket, GNAT.Sockets.IP_Protocol_For_IP_Level,
        (GNAT.Sockets.Add_Membership, Multicast_Address,
          GNAT.Sockets.Any_Inet_Addr));

   end Join_Multicast_Group;

   procedure Send
     (This :     Ethernet_Type; Address : Address_V4_Type; Port : Port_Type;
      Data :     Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is

      Destination_Address : GNAT.Sockets.Sock_Addr_Type;

   begin

      -- set the destination address
      Destination_Address :=
        GNAT.Sockets.Sock_Addr_Type'
          (Family => GNAT.Sockets.Family_Inet,
           Addr   =>
             GNAT.Sockets.Inet_Addr_Type'
               (Family => GNAT.Sockets.Family_Inet,
                Sin_V4 =>
                  GNAT.Sockets.Inet_Addr_V4_Type'
                    (1 => GNAT.Sockets.Inet_Addr_Comp_Type (Address (1)),
                     2 => GNAT.Sockets.Inet_Addr_Comp_Type (Address (2)),
                     3 => GNAT.Sockets.Inet_Addr_Comp_Type (Address (3)),
                     4 => GNAT.Sockets.Inet_Addr_Comp_Type (Address (4)))),
           Port   => GNAT.Sockets.Port_Type (Port));

      -- send the data
      GNAT.Sockets.Send_Socket (This.Socket, Data, Last, Destination_Address);

   end Send;

   procedure Receive
     (This :     Ethernet_Type; Address : out Address_V4_Type;
      Port : out Port_Type; Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is

      Source_Address : GNAT.Sockets.Sock_Addr_Type;

   begin

      -- get the data
      GNAT.Sockets.Receive_Socket (This.Socket, Data, Last, Source_Address);

      -- set the source address
      Address :=
        Address_V4_Type'
          (1 => Address_Octet_Type (Source_Address.Addr.Sin_V4 (1)),
           2 => Address_Octet_Type (Source_Address.Addr.Sin_V4 (2)),
           3 => Address_Octet_Type (Source_Address.Addr.Sin_V4 (3)),
           4 => Address_Octet_Type (Source_Address.Addr.Sin_V4 (4)));

      Port := Port_Type (Source_Address.Port);

   end Receive;

   function Is_New_Data (This : Ethernet_Type) return Boolean is

      use type GNAT.Sockets.Selector_Status;

      R_Socket_Set : GNAT.Sockets.Socket_Set_Type;
      W_Socket_Set : GNAT.Sockets.Socket_Set_Type;

      Status  : GNAT.Sockets.Selector_Status;
      Timeout : constant Duration := 0.0;

   begin

      GNAT.Sockets.Set (R_Socket_Set, This.Socket);

      -- check the selector
      GNAT.Sockets.Check_Selector
        (This.Selector, R_Socket_Set, W_Socket_Set, Status, Timeout);

      -- return the result
      return Status = GNAT.Sockets.Completed;

   end Is_New_Data;

end Drivers.Ethernet;
