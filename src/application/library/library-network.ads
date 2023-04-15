with Ada.Real_Time;
with Interfaces;

with Drivers.Ethernet;

with Types.Schedule;

package Library.Network is

   type Transport_Type is (Radio, Cloud);

   type Device_Identifier_Type is mod 2**16;
   for Device_Identifier_Type'Size use 16;
   Device_Identifier_Default : constant Device_Identifier_Type := 0;

   type Device_Identifier_Index_Type is range 1 .. 100;
   Device_Identifier_Index_Default : constant Device_Identifier_Index_Type :=
     1;

   type Device_Identifier_Array_Type is
     array (Device_Identifier_Index_Type) of Device_Identifier_Type;
   Device_Identifier_Array_Default : constant Device_Identifier_Array_Type :=
     Device_Identifier_Array_Type'(others => Device_Identifier_Default);

   -- Stores mapping of devices identifiers to their IP addresses

   type Device_Address_Index_Type is range 1 .. 100;
   Device_Address_Index_Default : constant Device_Address_Index_Type := 1;

   type Device_Address_Type is record
      Identifier : Device_Identifier_Type;
      Address    : Drivers.Ethernet.Address_V4_Type;
   end record;
   Device_Address_Default : constant Device_Address_Type :=
     Device_Address_Type'
       (Identifier => Device_Identifier_Default,
        Address    => Drivers.Ethernet.Address_V4_Default);

   type Device_Address_Array_Type is
     array (Device_Address_Index_Type) of Device_Address_Type;
   Device_Address_Array_Default : constant Device_Address_Array_Type :=
     Device_Address_Array_Type'(others => Device_Address_Default);

   type Device_Address_Transport_Array_Type is
     array (Transport_Type) of Device_Address_Array_Type;
   Device_Address_Transport_Array_Default :
     constant Device_Address_Transport_Array_Type :=
     Device_Address_Transport_Array_Type'
       (others => Device_Address_Array_Default);

   -- Store the list of devices currently connected to the network

   type Connected_Device_Index_Type is range 1 .. 50;
   Connected_Device_Index_Default : constant Connected_Device_Index_Type := 1;

   type Connected_Device_Type is record
      Identifier : Device_Identifier_Type;
      Active     : Boolean;
      Time       : Ada.Real_Time.Time;
   end record;
   Connected_Device_Default : constant Connected_Device_Type :=
     Connected_Device_Type'
       (Identifier => Device_Identifier_Default, Active => False,
        Time       => Ada.Real_Time.Clock);

   type Connected_Device_Array_Type is
     array (Connected_Device_Index_Type) of Connected_Device_Type;
   Connected_Device_Array_Default : constant Connected_Device_Array_Type :=
     Connected_Device_Array_Type'(others => Connected_Device_Default);

   type Connected_Device_Transport_Array_Type is
     array (Transport_Type) of Connected_Device_Array_Type;
   Connected_Device_Transport_Array_Default :
     constant Connected_Device_Transport_Array_Type :=
     Connected_Device_Transport_Array_Type'
       (others => Connected_Device_Array_Default);

   -- Define how packets are structured

   type Packet_Variant_Type is (Alive, Telemetry, Command, Response, Unknown);
   for Packet_Variant_Type use
     (Alive => 0, Telemetry => 1, Command => 2, Response => 3, Unknown => 4);
   for Packet_Variant_Type'Size use 4;
   Packet_Variant_Default : constant Packet_Variant_Type := Unknown;

   type Packet_Number_Type is mod 2**8;
   for Packet_Number_Type'Size use 8;
   Packet_Number_Default : constant Packet_Number_Type := 0;

   type Payload_Index_Type is mod 2**10;
   for Payload_Index_Type'Size use 10;
   Payload_Index_Default : constant Payload_Index_Type := 0;

   type Payload_Array_Type is
     array (Payload_Index_Type) of Interfaces.Unsigned_8;
   for Payload_Array_Type'Size use 8_192;
   Payload_Array_Default : constant Payload_Array_Type :=
     Payload_Array_Type'(others => 0);

   type My_Boolean is new Boolean;
   for My_Boolean'Size use 1;

   type Packet_Type is record
      Packet_Variant : Packet_Variant_Type;
      Packet_Number  : Packet_Number_Type;
      Broadcast      : My_Boolean;
      Source         : Device_Identifier_Type;
      Target         : Device_Identifier_Type;
      Payload_Length : Payload_Index_Type;
      Payload        : Payload_Array_Type;
   end record;
   for Packet_Type use record
      Packet_Variant at 0 * 16 range  0 ..     3;
      Packet_Number  at 0 * 16 range  4 ..    11;
      Broadcast      at 0 * 16 range 12 ..    12;
      Source         at 0 * 16 range 13 ..    28;
      Target         at 0 * 16 range 29 ..    44;
      Payload_Length at 0 * 16 range 45 ..    55;
      Payload        at 0 * 16 range 56 .. 8_247;
   end record;
   for Packet_Type'Size use 8_248;
   Packet_Default : constant Packet_Type :=
     Packet_Type'
       (Packet_Variant => Packet_Variant_Default,
        Packet_Number  => Packet_Number_Default,
        Source         => Device_Identifier_Default,
        Target         => Device_Identifier_Default,
        Payload_Length => Payload_Index_Default,
        Payload        => Payload_Array_Default, Broadcast => False);

   type Packet_Index_Type is mod 2**5;
   Packet_Index_Default : constant Packet_Index_Type := 0;

   type Packet_Array_Type is array (Packet_Index_Type) of Packet_Type;
   Packet_Array_Default : constant Packet_Array_Type :=
     Packet_Array_Type'(others => Packet_Default);

   type Packet_Collection_Type is record
      Packet_Index : Packet_Index_Type;
      Packet_Array : Packet_Array_Type;
   end record;
   Packet_Collection_Default : constant Packet_Collection_Type :=
     Packet_Collection_Type'
       (Packet_Index => Packet_Index_Default,
        Packet_Array => Packet_Array_Default);

   type Packet_Collection_Array_Type is
     array (Packet_Variant_Type) of Packet_Collection_Type;
   Packet_Collection_Array_Default : constant Packet_Collection_Array_Type :=
     Packet_Collection_Array_Type'(others => Packet_Collection_Default);

   type Network_Type
     (Device_Identifier : Device_Identifier_Type;
      Radio             : Drivers.Ethernet.Ethernet_Access_Type;
      Cloud             : Drivers.Ethernet.Ethernet_Access_Type)
   is
     tagged private;

   type Network_Access_Type is access Network_Type'Class;

   procedure Initialize
     (This                    : in out Network_Type;
      Radio_Multicast_Address :        Drivers.Ethernet.Address_V4_Type;
      Cloud_Server_Address    :        Drivers.Ethernet.Address_V4_Type);

   procedure Schedule
     (This : in out Network_Type; Cycle : Types.Schedule.Cycle_Type);

   procedure Send_Packet (This : in out Network_Type; Packet : Packet_Type);

   function Get_Packet
     (This : in out Network_Type; Variant : Packet_Variant_Type)
      return Packet_Type;

   function Get_Connected
     (This         : in out Network_Type; Transport : Transport_Type;
      Device_Index : Connected_Device_Index_Type) return Connected_Device_Type;

private

   type Network_Type
     (Device_Identifier : Device_Identifier_Type;
      Radio             : Drivers.Ethernet.Ethernet_Access_Type;
      Cloud             : Drivers.Ethernet.Ethernet_Access_Type)
   is
   tagged record
      Radio_Multicast_Address : Drivers.Ethernet.Address_V4_Type;
      Cloud_Server_Address    : Drivers.Ethernet.Address_V4_Type;

      Device_Address_Transport_Array   : Device_Address_Transport_Array_Type :=
        Device_Address_Transport_Array_Default;
      Connected_Device_Transport_Array : Connected_Device_Transport_Array_Type :=
        Connected_Device_Transport_Array_Default;
      Connected_Device_Timeout         : Ada.Real_Time.Time_Span :=
        Ada.Real_Time.Seconds (5);

      Packet_Collection : Packet_Collection_Array_Type :=
        Packet_Collection_Array_Default;
   end record;

end Library.Network;
