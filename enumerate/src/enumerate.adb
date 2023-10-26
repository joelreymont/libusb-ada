pragma Ada_2022;

with Ada.Text_IO;
use Ada.Text_IO;
with USB;
use USB;

procedure Enumerate is
begin

   USB.Init;

   declare

      Ctx        : constant USB.Context     := USB.Make_Context;
      Devices    : constant USB.Device_List := USB.Get_Device_List (Ctx);
      Descriptor : USB.Device_Descriptor;

   begin

      for Device of Devices loop
         Put_Line ("Device: " & Devices'Image);
         Descriptor := USB.Get_Device_Descriptor (Device);
         Put_Line
           ("Vendor ID: " & USB.Hex (Descriptor.Vendor_ID) & ", Product ID: " &
            USB.Hex (Descriptor.Product_ID));

      end loop;

   end;

end Enumerate;
