with Ada.Streams;
with GNAT.Sockets;

package Drivers.Ethernet is

   type Address_Octet_Type is range 0 .. 255;
   Address_Octet_Default : constant Address_Octet_Type := 0;

   type Address_V0_Type is array (Natural range <>) of Address_Octet_Type;

   type Address_V4_Type is new Address_V0_Type (1 .. 4);
   Address_V4_Default : constant Address_V4_Type := (others => 0);

   type Address_V6_Type is new Address_V0_Type (1 .. 16);
   Address_V6_Default : constant Address_V6_Type := (others => 0);

   type Address_V4_Access_Type is access Address_V4_Type;
   type Address_V6_Access_Type is access Address_V6_Type;

   type Port_Type is range 1 .. 65_535;
   Port_Default : constant Port_Type := Port_Type'Last;

   type Address_Port_Type is record
      Address : Address_V4_Type;
      Port    : Port_Type;
   end record;
   Address_Port_Default : constant Address_Port_Type :=
     (Address => Address_V4_Default, Port => Port_Default);

   type Ethernet_Type (Address : Address_V4_Access_Type; Port : Port_Type) is
     tagged private;

   type Ethernet_Access_Type is access Ethernet_Type'Class;

   procedure Initialize (This : in out Ethernet_Type);

   procedure Join_Multicast_Group
     (This : in out Ethernet_Type; Address : Address_V4_Type);

   procedure Send
     (This :     Ethernet_Type; Address : Address_V4_Type; Port : Port_Type;
      Data :     Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   procedure Receive
     (This :     Ethernet_Type; Address : out Address_V4_Type;
      Port : out Port_Type; Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   function Is_New_Data (This : Ethernet_Type) return Boolean;

private

   type Ethernet_Type (Address : Address_V4_Access_Type; Port : Port_Type) is
   tagged limited record
      Socket   : GNAT.Sockets.Socket_Type;
      Selector : GNAT.Sockets.Selector_Type;
   end record;

end Drivers.Ethernet;
