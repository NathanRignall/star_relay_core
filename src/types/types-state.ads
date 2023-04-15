with Ada.Real_Time;

with Types.Physics;

package Types.State is

  type Physical_State_Type is record
    Position                    : Types.Physics.Position_Type;
    Acceleration_Vector         : Types.Physics.Acceleration_Vector_Type;
    Velocity_Vector             : Types.Physics.Velocity_Vector_Type;
    Rotation_Vector             : Types.Physics.Rotation_Vector_Type;
    Angular_Velocity_Vector     : Types.Physics.Angular_Velocity_Vector_Type;
    Angular_Acceleration_Vector : Types.Physics
     .Angular_Acceleration_Vector_Type;
  end record;
  Physical_State_Default : constant Physical_State_Type :=
   (Position                    => Types.Physics.Position_Default,
    Acceleration_Vector         => Types.Physics.Acceleration_Vector_Default,
    Velocity_Vector             => Types.Physics.Velocity_Vector_Default,
    Rotation_Vector             => Types.Physics.Rotation_Vector_Default,
    Angular_Velocity_Vector => Types.Physics.Angular_Velocity_Vector_Default,
    Angular_Acceleration_Vector =>
     Types.Physics.Angular_Acceleration_Vector_Default);

  type Core_State_Type is record
    Physical_State : Physical_State_Type;
  end record;
  Core_State_Default : constant Core_State_Type :=
   (Physical_State => Physical_State_Default);

  type Aircraft_Identifier_Type is mod 2**16;
  Aircraft_Identifier_Default : constant Aircraft_Identifier_Type := 0;

  type Aircraft_Type is record
    Identifier      : Aircraft_Identifier_Type;
    Position        : Types.Physics.Position_Type;
    Velocity_Vector : Types.Physics.Velocity_Vector_Type;
    Rotation_Vector : Types.Physics.Rotation_Vector_Type;
    Active          : Boolean;
    Time            : Ada.Real_Time.Time;
  end record;
  Aircraft_Default : constant Aircraft_Type :=
   (Identifier      => Aircraft_Identifier_Default,
    Position        => Types.Physics.Position_Default,
    Velocity_Vector => Types.Physics.Velocity_Vector_Default,
    Rotation_Vector => Types.Physics.Rotation_Vector_Default, Active => False,
    Time            => Ada.Real_Time.Clock);

  type Aircraft_Index_Type is range 1 .. 100;
  Aircraft_Index_Default : constant Aircraft_Index_Type := 1;

  type Aircraft_Array_Type is array (Aircraft_Index_Type) of Aircraft_Type;
  Aircraft_Array_Default : constant Aircraft_Array_Type :=
   (others => Aircraft_Default);

end Types.State;
