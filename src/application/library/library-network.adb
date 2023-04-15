with Ada.Streams;
with Ada.Text_IO;

package body Library.Network is

   procedure Recieve_Process_Packets (This : in out Network_Type);

   procedure Receive_Packet
     (This                : in out Network_Type; Transport : Transport_Type;
      Packet              :    out Packet_Type;
      Source_Address_Port :    out Drivers.Ethernet.Address_Port_Type);

   procedure Process_Packet
     (This                : in out Network_Type; Transport : Transport_Type;
      Packet              :        Packet_Type;
      Source_Address_Port :        Drivers.Ethernet.Address_Port_Type);

   procedure Process_Alive_Packet
     (This                : in out Network_Type; Transport : Transport_Type;
      Packet              :        Packet_Type;
      Source_Address_Port :        Drivers.Ethernet.Address_Port_Type);

   procedure Send_Packet_Alive (This : in out Network_Type);

   procedure Cleanup_Connected_Device_Array (This : in out Network_Type);

   procedure Lookup_Address
     (This              :     Network_Type; Transport : Transport_Type;
      Device_Identifier :     Device_Identifier_Type;
      Device_Address    : out Drivers.Ethernet.Address_V4_Type);

   function Is_Connected
     (This              : in out Network_Type; Transport : Transport_Type;
      Device_Identifier :        Device_Identifier_Type) return Boolean;

   procedure Initialize
     (This                    : in out Network_Type;
      Radio_Multicast_Address :        Drivers.Ethernet.Address_V4_Type;
      Cloud_Server_Address    :        Drivers.Ethernet.Address_V4_Type)
   is
   begin

      Ada.Text_IO.Put_Line ("Network_Type Initialize");
      Ada.Text_IO.Put_Line ("Device Id" & This.Device_Identifier'Image);

      -- set the variables
      This.Radio_Multicast_Address := Radio_Multicast_Address;
      This.Cloud_Server_Address    := Cloud_Server_Address;

   end Initialize;

   procedure Schedule
     (This : in out Network_Type; Cycle : Types.Schedule.Cycle_Type)
   is
   begin

      case Cycle is

         when Types.Schedule.S_20ms =>
            This.Recieve_Process_Packets;

         when Types.Schedule.S_500ms =>
            This.Cleanup_Connected_Device_Array;

         when Types.Schedule.S_2000ms =>
            This.Send_Packet_Alive;

         when others =>
            null;

      end case;

   end Schedule;

   procedure Recieve_Process_Packets (This : in out Network_Type) is

      Packet              : Packet_Type;
      Source_Address_Port : Drivers.Ethernet.Address_Port_Type;

   begin
      -- loop for a maximum of 100 times for radio
      for Index in 1 .. 100 loop

         -- check if there is a new packet
         if This.Radio.all.Is_New_Data then

            -- get the packet
            Receive_Packet (This, Radio, Packet, Source_Address_Port);

            -- process the packet
            Process_Packet (This, Radio, Packet, Source_Address_Port);

         else

            -- exit the loop as there is no new data
            exit;

         end if;

      end loop;

      -- loop for a maximum of 100 times for cloud
      for Index in 1 .. 100 loop

         -- check if there is a new packet
         if This.Cloud.Is_New_Data then

            -- get the packet
            This.Receive_Packet (Cloud, Packet, Source_Address_Port);

            -- process the packet
            This.Process_Packet (Cloud, Packet, Source_Address_Port);

         else

            -- exit the loop as there is no new data
            exit;

         end if;

      end loop;
   end Recieve_Process_Packets;

   procedure Receive_Packet
     (This                : in out Network_Type; Transport : Transport_Type;
      Packet              :    out Packet_Type;
      Source_Address_Port :    out Drivers.Ethernet.Address_Port_Type)
   is

      Data : Ada.Streams.Stream_Element_Array (1 .. 1_031) := (others => 0);
      Last : Ada.Streams.Stream_Element_Offset;

      New_Packet : Packet_Type;
      for New_Packet'Address use Data'Address;

   begin

      -- get the packet from transport
      case Transport is

         when Radio =>
            This.Radio.Receive
              (Source_Address_Port.Address, Source_Address_Port.Port, Data,
               Last);

         when Cloud =>
            This.Cloud.Receive
              (Source_Address_Port.Address, Source_Address_Port.Port, Data,
               Last);

      end case;

      -- overlay the data on the packet
      Packet := New_Packet;

   end Receive_Packet;

   procedure Process_Packet
     (This                : in out Network_Type; Transport : Transport_Type;
      Packet              :        Packet_Type;
      Source_Address_Port :        Drivers.Ethernet.Address_Port_Type)
   is
   begin

      if Packet.Packet_Variant'Valid then

         case Packet.Packet_Variant is

            when Alive =>
               Process_Alive_Packet
                 (This, Transport, Packet, Source_Address_Port);

            when Telemetry =>
               This.Packet_Collection (Telemetry).Packet_Index     :=
                 Packet_Index_Type'Succ
                   (This.Packet_Collection (Telemetry).Packet_Index);
               This.Packet_Collection (Telemetry).Packet_Array
                 (This.Packet_Collection (Telemetry).Packet_Index) :=
                 Packet;

            when Command =>
               Ada.Text_IO.Put_Line
                 ("Request:" & Packet.Source'Image & " Packet:" &
                  Packet.Packet_Number'Image & " Transport: " &
                  Transport'Image);

               This.Packet_Collection (Command).Packet_Index     :=
                 Packet_Index_Type'Succ
                   (This.Packet_Collection (Command).Packet_Index);
               This.Packet_Collection (Command).Packet_Array
                 (This.Packet_Collection (Command).Packet_Index) :=
                 Packet;

            when Response =>
               Ada.Text_IO.Put_Line
                 ("Response" & Packet.Source'Image & " Packet:" &
                  Packet.Packet_Number'Image & " Transport: " &
                  Transport'Image);

               This.Packet_Collection (Response).Packet_Index     :=
                 Packet_Index_Type'Succ
                   (This.Packet_Collection (Response).Packet_Index);
               This.Packet_Collection (Response).Packet_Array
                 (This.Packet_Collection (Response).Packet_Index) :=
                 Packet;

            when Unknown =>
               Ada.Text_IO.Put_Line
                 ("Unknown" & Packet.Source'Image & " Packet:" &
                  Packet.Packet_Number'Image & " Transport: " &
                  Transport'Image);

         end case;

      else

         Ada.Text_IO.Put_Line ("Invalid");

      end if;

   end Process_Packet;

   procedure Process_Alive_Packet
     (This                : in out Network_Type; Transport : Transport_Type;
      Packet              :        Packet_Type;
      Source_Address_Port :        Drivers.Ethernet.Address_Port_Type)
   is

      use type Drivers.Ethernet.Address_V4_Type;

      No_Space : exception;

   begin

      -- check if the device is already connected
      if not Is_Connected (This, Transport, Packet.Source) then

         -- add the device to the connected device array
         for Device_Index in Connected_Device_Index_Type loop

            -- check if space is available
            if not This.Connected_Device_Transport_Array (Transport)
                (Device_Index)
                .Active
            then

               -- add the device to the connected device array
               This.Connected_Device_Transport_Array (Transport)
                 (Device_Index) :=
                 Connected_Device_Type'
                   (Identifier => Packet.Source, Active => True,
                    Time       => Ada.Real_Time.Clock);

               -- add the device to the device address array
               for Device_Address_Index in Device_Address_Index_Type loop

                  -- check if space is available
                  if This.Device_Address_Transport_Array (Transport)
                      (Device_Address_Index)
                      .Identifier =
                    0
                  then

                     -- write to the console the device that has connected
                     Ada.Text_IO.Put_Line
                       ("Add IP:" & Source_Address_Port.Address (1)'Image &
                        "." & Source_Address_Port.Address (2)'Image & "." &
                        Source_Address_Port.Address (3)'Image & "." &
                        Source_Address_Port.Address (4)'Image & " ID:" &
                        Packet.Source'Image & " Transport: " &
                        Transport'Image);

                     -- add the device to the device address array
                     This.Device_Address_Transport_Array (Transport)
                       (Device_Address_Index) :=
                       Device_Address_Type'
                         (Identifier => Packet.Source,
                          Address    => Source_Address_Port.Address);

                     return;

                  elsif Device_Address_Index = Device_Address_Index_Type'Last
                  then

                     -- exception
                     raise No_Space with "No Space in Device Address Array";

                  end if;

               end loop;

            elsif Device_Index = Connected_Device_Index_Type'Last then

               -- exception
               raise No_Space with "No Space in Connected Device Array";

            end if;

         end loop;

      else

         -- update the time
         for Device_Index in Connected_Device_Index_Type loop

            -- check if currently on the device
            if This.Connected_Device_Transport_Array (Transport) (Device_Index)
                .Identifier =
              Packet.Source
            then

               -- update the time
               This.Connected_Device_Transport_Array (Transport) (Device_Index)
                 .Time :=
                 Ada.Real_Time.Clock;

               -- check the ip address
               for Device_Address_Index in Device_Address_Index_Type loop

                  -- check if currently on the device
                  if This.Device_Address_Transport_Array (Transport)
                      (Device_Address_Index)
                      .Identifier =
                    Packet.Source
                  then

                     -- check if the ip address has changed
                     if This.Device_Address_Transport_Array (Transport)
                         (Device_Address_Index)
                         .Address /=
                       Source_Address_Port.Address
                     then

                        -- write to the console the device that has connected
                        Ada.Text_IO.Put_Line
                          ("Update IP:" &
                           Source_Address_Port.Address (1)'Image & "." &
                           Source_Address_Port.Address (2)'Image & "." &
                           Source_Address_Port.Address (3)'Image & "." &
                           Source_Address_Port.Address (4)'Image & " ID:" &
                           Packet.Source'Image & " Transport: " &
                           Transport'Image);

                        -- update the ip address
                        This.Device_Address_Transport_Array (Transport)
                          (Device_Address_Index)
                          .Address :=
                          Source_Address_Port.Address;

                     end if;

                     -- exit the loop as the device has been found and updated
                     return;

                  elsif Device_Address_Index = Device_Address_Index_Type'Last
                  then

                     -- exception
                     raise No_Space with "Device Address Not Found";

                  end if;

               end loop;

            elsif Device_Index = Connected_Device_Index_Type'Last then

               -- exception
               raise No_Space with "Device Not Found";

            end if;

         end loop;

      end if;

   end Process_Alive_Packet;

   function Get_Packet
     (This : in out Network_Type; Variant : Packet_Variant_Type)
      return Packet_Type
   is

      New_Packet : Packet_Type := Packet_Default;

   begin

      -- get the packet from the packet array
      New_Packet :=
        This.Packet_Collection (Variant).Packet_Array
          (This.Packet_Collection (Variant).Packet_Index);

      -- clear the packet from the packet array
      This.Packet_Collection (Variant).Packet_Array
        (This.Packet_Collection (Variant).Packet_Index) :=
        Packet_Default;

      return New_Packet;

   end Get_Packet;

   procedure Send_Packet (This : in out Network_Type; Packet : Packet_Type) is

      Destination_Address : Drivers.Ethernet.Address_V4_Type := (127, 0, 0, 1);
      Last     : Ada.Streams.Stream_Element_Offset;

      -- this needs to change set to address of procedure parameter
      New_Data : Ada.Streams.Stream_Element_Array (1 .. 1_024);
      for New_Data'Address use Packet'Address;

   begin

      -- check if a broadcast
      if not Packet.Broadcast then

         -- check if the intended destination is direct
         if Is_Connected (This, Radio, Packet.Target) then

            -- lookup the ip
            Lookup_Address (This, Radio, Packet.Target, Destination_Address);

            -- send the packet
            This.Radio.Send
              (Address => Destination_Address, Port => This.Radio.Port,
               Data    => New_Data, Last => Last);

         elsif Is_Connected (This, Cloud, Packet.Target) then

            -- lookup the ip
            Lookup_Address (This, Cloud, Packet.Target, Destination_Address);

            -- send the packet
            This.Cloud.Send
              (Address => Destination_Address, Port => This.Cloud.Port,
               Data    => New_Data, Last => Last);

         else

            -- send the packet to cloud server
            This.Cloud.Send
              (Address => This.Cloud_Server_Address, Port => This.Cloud.Port,
               Data    => New_Data, Last => Last);

         end if;

      else

         -- send the packet on radio multicast
         This.Radio.Send
           (Address => This.Radio_Multicast_Address, Port => This.Radio.Port,
            Data    => New_Data, Last => Last);

         -- send the packet to cloud server
         This.Cloud.Send
           (Address => This.Cloud_Server_Address, Port => This.Cloud.Port,
            Data    => New_Data, Last => Last);

      end if;

   end Send_Packet;

   procedure Send_Packet_Alive (This : in out Network_Type) is

      Packet_Alive : Packet_Type;

   begin

      -- create the alive packet
      Packet_Alive :=
        Packet_Type'
          (Packet_Variant => Alive, Packet_Number => 0,
           Source => This.Device_Identifier, Target => 0, Payload_Length => 0,
           Payload        => Payload_Array_Default, Broadcast => True);

      -- send the packet
      Send_Packet (This, Packet_Alive);

   end Send_Packet_Alive;

   procedure Cleanup_Connected_Device_Array (This : in out Network_Type) is

      use type Ada.Real_Time.Time;

      Current_Time : Ada.Real_Time.Time;
      Timeout_Time : Ada.Real_Time.Time;
      
   begin

      -- loop through the transport array
      for Transport in Transport_Type loop

         -- loop through the connected device array
         for Device_Index in Connected_Device_Index_Type loop

            -- check if the device is active
            if This.Connected_Device_Transport_Array (Transport) (Device_Index)
                .Active
            then

               -- get the current time
               Current_Time := Ada.Real_Time.Clock;

               -- get the timeout time
               Timeout_Time :=
                 This.Connected_Device_Transport_Array (Transport)
                   (Device_Index)
                   .Time +
                 This.Connected_Device_Timeout;

               -- check if the device has timed out
               if Current_Time > Timeout_Time then

                  -- loop through the device address array to find the device
                  for Device_Address_Index in Device_Address_Index_Type loop

                     -- check if the device is found
                     if This.Device_Address_Transport_Array (Transport)
                         (Device_Address_Index)
                         .Identifier =
                       This.Connected_Device_Transport_Array (Transport)
                         (Device_Index)
                         .Identifier
                     then

                        -- write to the console that the device has timed out
                        Ada.Text_IO.Put_Line
                          ("Remove IP:" &
                           This.Device_Address_Transport_Array (Transport)
                             (Device_Address_Index)
                             .Address
                             (1)'
                             Image &
                           "." &
                           This.Device_Address_Transport_Array (Transport)
                             (Device_Address_Index)
                             .Address
                             (2)'
                             Image &
                           "." &
                           This.Device_Address_Transport_Array (Transport)
                             (Device_Address_Index)
                             .Address
                             (3)'
                             Image &
                           "." &
                           This.Device_Address_Transport_Array (Transport)
                             (Device_Address_Index)
                             .Address
                             (4)'
                             Image &
                           "." & " ID:" &
                           This.Connected_Device_Transport_Array (Transport)
                             (Device_Index)
                             .Identifier'
                             Image &
                           " Transport: " & Transport'Image);

                        -- remove the device from the device address array
                        This.Device_Address_Transport_Array (Transport)
                          (Device_Address_Index) :=
                          Device_Address_Default;

                     end if;

                  end loop;

                  -- remove the device from the connected device array (after address has been removed)
                  This.Connected_Device_Transport_Array (Transport)
                    (Device_Index) :=
                    Connected_Device_Default;

               end if;

            end if;

         end loop;

      end loop;

   end Cleanup_Connected_Device_Array;

   procedure Lookup_Address
     (This              :     Network_Type; Transport : Transport_Type;
      Device_Identifier :     Device_Identifier_Type;
      Device_Address    : out Drivers.Ethernet.Address_V4_Type)
   is
   begin

      -- loop through the device address array
      for Device_Address_Index in Device_Address_Index_Type loop

         -- check if the device identifier matches
         if This.Device_Address_Transport_Array (Transport)
             (Device_Address_Index)
             .Identifier =
           Device_Identifier
         then

            -- set the device address
            Device_Address :=
              This.Device_Address_Transport_Array (Transport)
                (Device_Address_Index)
                .Address;

            -- exit the loop as the device address has been found
            return;

         end if;

      end loop;

   end Lookup_Address;

   function Is_Connected
     (This              : in out Network_Type; Transport : Transport_Type;
      Device_Identifier :        Device_Identifier_Type) return Boolean
   is
   begin

      -- loop through the connected device array
      for Device_Index in Connected_Device_Index_Type loop

         -- check if the device is active and the device identifier matches
         if This.Connected_Device_Transport_Array (Transport) (Device_Index)
             .Active
           and then
             This.Connected_Device_Transport_Array (Transport) (Device_Index)
               .Identifier =
             Device_Identifier
         then

            -- device is connected
            return True;

         end if;

      end loop;

      -- device is not connected as not found
      return False;

   end Is_Connected;

   function Get_Connected
     (This         : in out Network_Type; Transport : Transport_Type;
      Device_Index : Connected_Device_Index_Type) return Connected_Device_Type
   is
   begin
      return This.Connected_Device_Transport_Array (Transport) (Device_Index);
   end Get_Connected;

end Library.Network;
