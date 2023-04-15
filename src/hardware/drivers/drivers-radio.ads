package Drivers.Radio is

   type Radio_Type is tagged private;

   type Radio_Access_Type is access Radio_Type'Class;

   procedure Initialize (This : in out Radio_Type);

private

   type Radio_Type is tagged record

      null;

   end record;

end Drivers.Radio;
