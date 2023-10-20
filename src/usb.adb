pragma Ada_2022;

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with USB.Low;
with System.Address_Image;
with Util.Log;
with Util.Log.Loggers;

package body USB is

   use Util.Log;

   package C renames Interfaces.C;

   Log : Loggers.Logger := Loggers.Create ("LIBUSB_ADA");

   procedure Init is
   begin
      Loggers.Initialize ("libusb-log4j.properties");
      Log.Set_Level (DEBUG_LEVEL);
      Log.Info ("Starting the log example, level is {0}", Log.Get_Level_Name);
   end Init;

   function Error_Text (Error_Code : C.int) return String is

      Text : constant C.Strings.chars_ptr := USB.Low.Error_Text (Error_Code);

   begin

      return C.Strings.Value (Text);

   end Error_Text;

   procedure Check_Error (Error_Code : C.int) is

      function Error_From_Code is new Ada.Unchecked_Conversion
        (C.int, USB.Low.Error);
      Err  : constant USB.Low.Error := Error_From_Code (Error_Code);
      Text : constant String        := Error_Text (Error_Code);

   begin

      if not Err'Valid then
         return;
      end if;

      case Err is
         when USB.Low.Other_Error =>
            raise Other_Error with Text;
         when USB.Low.Operation_Not_Supported_Error =>
            raise Operation_Not_Supported_Error with Text;
         when USB.Low.Out_Of_Memory_Error =>
            raise Out_Of_Memory_Error with Text;
         when USB.Low.Syscall_Interrupted_Error =>
            raise Syscall_Interrupted_Error with Text;
         when USB.Low.Pipe_Error =>
            raise Pipe_Error with Text;
         when USB.Low.Overflow_Error =>
            raise Overflow_Error with Text;
         when USB.Low.Timeout_Error =>
            raise Timeout_Error with Text;
         when USB.Low.Resource_Busy_Error =>
            raise Resource_Busy_Error with Text;
         when USB.Low.Not_Found_Error =>
            raise Not_Found_Error with Text;
         when USB.Low.No_Device_Error =>
            raise No_Device_Error with Text;
         when USB.Low.Access_Denied_Error =>
            raise Access_Denied_Error with Text;
         when USB.Low.Invalid_Parameter_Error =>
            raise Invalid_Parameter_Error with Text;
         when USB.Low.Input_Output_Error =>
            raise Input_Output_Error with Text;
         when others =>
            null;
      end case;

   end Check_Error;

   function Make_Context return Context is

      Ctx  : Context;
      Data : Context_Data;

   begin

      Check_Error (USB.Low.Init (Data.Address));
      Ctx.Set (Data);

      return Ctx;

   end Make_Context;

   procedure Context_Data_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  :        Context_Data)
   is
   begin

      Output.Put ("Context_Data => " & System.Address_Image (Value.Address));

   end Context_Data_Put_Image;

   procedure Context_Release (Self : in out Context_Data) is
   begin

      USB.Low.Deinit (Self.Address);

   end Context_Release;

   function Get_Device_List (Ctx : Context'Class) return Device_List is

      Data              : Device_List_Data;
      Number_of_Devices : C.long;
      Devices           : Device_List;

   begin

      Number_of_Devices :=
        USB.Low.Get_DeviceList (Ctx.Get.Address, Data.Address);
      Check_Error (C.int (Number_of_Devices));
      Log.Debug
        ("Got device list {0} with {1} devices", Data'Image,
         Number_of_Devices'Image);
      Devices.Set (Data);

      return Devices;

   end Get_Device_List;

   procedure Device_List_Data_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  :        Device_List_Data)
   is
   begin

      Output.Put
        ("Device_List_Data => " & System.Address_Image (Value.Address));

   end Device_List_Data_Put_Image;

   procedure Device_List_Release (Self : in out Device_List_Data) is
   begin

      Log.Debug ("Freeing device list: {0}", Self'Image);
      USB.Low.Free_Device_List (Self.Address, 1);

   end Device_List_Release;

end USB;
