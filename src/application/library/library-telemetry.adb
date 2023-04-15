with Ada.Text_IO;
with Ada.Real_Time;

with Application.State;

package body Library.Telemetry is

   procedure Process_Telemetry (This : Telemetry_Type);

   procedure Process_Location_Telemetry
     (This   : Telemetry_Type; Telemetry_Packet : Telemetry_Packet_Type;
      Source : Library.Network.Device_Identifier_Type);

   procedure Send_Location (This : Telemetry_Type);

   procedure Initialize (This : Telemetry_Type) is
   begin

      Ada.Text_IO.Put_Line ("Telemetry Initialize");

   end Initialize;

   procedure Schedule
     (This : Telemetry_Type; Cycle : Types.Schedule.Cycle_Type)
   is
   begin

      case Cycle is

         when Types.Schedule.S_20ms =>
            This.Process_Telemetry;

         when Types.Schedule.S_200ms =>
            This.Send_Location;

         when others =>
            null;

      end case;

   end Schedule;

   procedure Process_Telemetry (This : Telemetry_Type) is

      use type Library.Network.Packet_Type;

      Packet  : Library.Network.Packet_Type;
      Payload : Library.Network.Payload_Array_Type;

      Variant : Variant_Type;
      for Variant'Address use Payload'Address;

   begin

      -- loop through all the packets (up to 100)
      for Index in 1 .. 100 loop

         -- get the next packet to process
         Packet := This.Network.Get_Packet (Library.Network.Telemetry);

         -- overlay the packet payload on the payload array
         Payload := Packet.Payload;

         -- check if packet is not default
         if Packet /= Library.Network.Packet_Default then

            declare

               Telemetry_Packet : Telemetry_Packet_Type (Variant);
               for Telemetry_Packet'Address use Payload'Address;

            begin

               -- overlay the packet payload on the payload array (again)
               Payload := Packet.Payload;

               -- check if variant is valid
               if Variant'Valid then

                  -- check the packet variant
                  case Variant is

                     when Location =>
                        Process_Location_Telemetry
                          (This, Telemetry_Packet, Packet.Source);

                     when others =>
                        Ada.Text_IO.Put_Line ("Telemetry Unknown");

                  end case;

               else

                  Ada.Text_IO.Put_Line ("Telemetry Invalid");

               end if;

            end;

         else

            -- no more packets to process so exit loop
            return;

         end if;

      end loop;

   end Process_Telemetry;

   procedure Process_Location_Telemetry
     (This   : Telemetry_Type; Telemetry_Packet : Telemetry_Packet_Type;
      Source : Library.Network.Device_Identifier_Type)
   is

      use type Types.State.Aircraft_Identifier_Type;
      use type Types.State.Aircraft_Index_Type;

      No_Space : exception;

   begin

      -- look if the aircraft is in the list of aircraft
      for Aircraft_Index in Types.State.Aircraft_Index_Type loop

         -- check if the aircraft is in the list of aircraft
         if Application.State.Aircraft_State (Aircraft_Index).Active = True
           and then
             Application.State.Aircraft_State (Aircraft_Index).Identifier =
             Types.State.Aircraft_Identifier_Type (Source)
         then

            -- update the aircraft state
            Application.State.Aircraft_State (Aircraft_Index).Position :=
              Telemetry_Packet.Location.Position;
            Application.State.Aircraft_State (Aircraft_Index)
              .Velocity_Vector                                         :=
              Telemetry_Packet.Location.Velocity_Vector;
            Application.State.Aircraft_State (Aircraft_Index)
              .Rotation_Vector                                         :=
              Telemetry_Packet.Location.Rotation_Vector;

            -- set the time of the last update
            Application.State.Aircraft_State (Aircraft_Index).Time :=
              Ada.Real_Time.Clock;

            -- exit the loop
            return;

         end if;

      end loop;

      -- if the aircraft is not in the list of aircraft, add it
      for Aircraft_Index in Types.State.Aircraft_Index_Type loop

         -- check if the solt does not have an an active aircraft
         if Application.State.Aircraft_State (Aircraft_Index).Active = False
         then

            -- place the aircraft in the list of aircraft
            Application.State.Aircraft_State (Aircraft_Index).Active := True;
            Application.State.Aircraft_State (Aircraft_Index).Identifier :=
              Types.State.Aircraft_Identifier_Type (Source);

            -- set the aircraft state
            Application.State.Aircraft_State (Aircraft_Index).Position :=
              Telemetry_Packet.Location.Position;
            Application.State.Aircraft_State (Aircraft_Index)
              .Velocity_Vector                                         :=
              Telemetry_Packet.Location.Velocity_Vector;
            Application.State.Aircraft_State (Aircraft_Index)
              .Rotation_Vector                                         :=
              Telemetry_Packet.Location.Rotation_Vector;

            -- set the time of the last update
            Application.State.Aircraft_State (Aircraft_Index).Time :=
              Ada.Real_Time.Clock;

            -- exit the loop
            return;

         elsif Aircraft_Index = Types.State.Aircraft_Index_Type'Last then

            -- exception
            raise No_Space with "No Space in Aircraft List";

         end if;

      end loop;

   end Process_Location_Telemetry;

   procedure Send_Location (This : Telemetry_Type) is

      Payload_Length : Library.Network.Payload_Index_Type := 1_023;
      Payload        : Library.Network.Payload_Array_Type := (others => 0);

      Telemetry_Packet : Telemetry_Packet_Type (Location);
      for Telemetry_Packet'Address use Payload'Address;

      New_Packet : Library.Network.Packet_Type;

   begin

      -- form the telemetry packet
      Telemetry_Packet :=
        Telemetry_Packet_Type'
          (Variant  => Location,
           Location =>
             Location_Type'
               (Position        =>
                  Application.State.Core_State.Physical_State.Position,
                Velocity_Vector =>
                  Application.State.Core_State.Physical_State.Velocity_Vector,
                Rotation_Vector =>
                  Application.State.Core_State.Physical_State
                    .Rotation_Vector));

      -- form the packet using the telemetry packet
      New_Packet :=
        Library.Network.Packet_Type'
          (Packet_Variant => Library.Network.Telemetry, Packet_Number => 45,
           Source         => This.Network.Device_Identifier, Target => 0,
           Payload_Length => Payload_Length, Payload => Payload,
           Broadcast      => Library.Network.False);

      -- send telemetry packet to all devices on radio network
      for Device_Index in Library.Network.Connected_Device_Index_Type loop

         -- check if the device is active
         if This.Network.Get_Connected (Library.Network.Radio, Device_Index)
             .Active
         then

            -- set the target
            New_Packet.Target :=
              This.Network.Get_Connected (Library.Network.Radio, Device_Index)
                .Identifier;

            -- send the packet
            This.Network.Send_Packet (New_Packet);

         end if;

      end loop;

      -- send telemetry to cloud server (temporary solution)
      New_Packet.Target := 0;
      This.Network.Send_Packet (New_Packet);

   end Send_Location;

end Library.Telemetry;
