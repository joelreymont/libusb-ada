pragma Ada_2022;

with Interfaces.C.Strings;
with System.Address_Image;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Util.Log;
with Util.Log.Loggers;

package body USB is

   use Util.Log;

   package C renames Interfaces.C;

   function Hex (n : in Integer) return String is
      Hex_Characters : constant String := "0123456789ABCDEF";
      Hexadecimal    : Unbounded_String;
      Remainder      : Integer         := n;
      I              : Integer         := 1;
   begin
      -- Check if the input integer is zero
      if Remainder = 0 then
         return "0";
      end if;

      -- Convert the integer to hexadecimal
      while Remainder > 0 loop
         Insert (Hexadecimal, 1, "" & Hex_Characters (Remainder mod 16 + 1));
         Remainder := Remainder / 16;
         I         := I + 1;
      end loop;

      return To_String (Hexadecimal);

   end Hex;

   type Device_List_Iterator (Data : access constant Device_Vectors.Vector) is
   new Device_List_Iterators.Forward_Iterator with null record;

   overriding function First
     (Object : Device_List_Iterator) return Device_List_Cursor;

   overriding function Next
     (Object   : Device_List_Iterator;
      Position : Device_List_Cursor)
      return Device_List_Cursor;

   Log : Loggers.Logger := Loggers.Create ("LIBUSB_ADA");

   procedure Init is
   begin
      Loggers.Initialize ("libusb-log4j.properties");
      Log.Set_Level (DEBUG_LEVEL);
      Log.Info ("Starting the log example, level is {0}", Log.Get_Level_Name);

   end Init;

   function Error_Text (Error_Code : C.int) return String is

      Text : constant C.Strings.chars_ptr := LibUSB.Error_Text (Error_Code);

   begin

      return C.Strings.Value (Text);

   end Error_Text;

   procedure Check_Error (Error_Code : C.int) is

      function Error_From_Code is new Ada.Unchecked_Conversion
        (C.int, LibUSB.Error);
      Err  : constant LibUSB.Error := Error_From_Code (Error_Code);
      Text : constant String       := Error_Text (Error_Code);

   begin

      if not Err'Valid then
         raise Other_Error with "Invalid error code: " & Error_Code'Image;
      end if;

      case Err is
         when LibUSB.Other_Error =>
            raise Other_Error with Text;
         when LibUSB.Operation_Not_Supported_Error =>
            raise Operation_Not_Supported_Error with Text;
         when LibUSB.Out_Of_Memory_Error =>
            raise Out_Of_Memory_Error with Text;
         when LibUSB.Syscall_Interrupted_Error =>
            raise Syscall_Interrupted_Error with Text;
         when LibUSB.Pipe_Error =>
            raise Pipe_Error with Text;
         when LibUSB.Overflow_Error =>
            raise Overflow_Error with Text;
         when LibUSB.Timeout_Error =>
            raise Timeout_Error with Text;
         when LibUSB.Resource_Busy_Error =>
            raise Resource_Busy_Error with Text;
         when LibUSB.Not_Found_Error =>
            raise Not_Found_Error with Text;
         when LibUSB.No_Device_Error =>
            raise No_Device_Error with Text;
         when LibUSB.Access_Denied_Error =>
            raise Access_Denied_Error with Text;
         when LibUSB.Invalid_Parameter_Error =>
            raise Invalid_Parameter_Error with Text;
         when LibUSB.Input_Output_Error =>
            raise Input_Output_Error with Text;
         when others =>
            null;
      end case;

   end Check_Error;

   function Make_Context return Context is

      Ctx : Context;
      Rc  : Context_Refcounted;

   begin

      Check_Error (LibUSB.Init (Rc.Context));
      Ctx.Set (Rc);

      return Ctx;

   end Make_Context;

   procedure Context_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Context_Refcounted) is
   begin

      Output.Put
        ("Context_Refcounted => " &
         System.Address_Image (Value.Context'Address));

   end Context_Put_Image;

   procedure Context_Release (Self : in out Context_Refcounted) is
   begin

      LibUSB.Deinit (Self.Context);

   end Context_Release;

   procedure Device_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Device_Refcounted) is
   begin

      Output.Put
        ("Device_Refcounted => " &
         System.Address_Image (Value.Device'Address));

   end Device_Put_Image;

   procedure Device_Release (Self : in out Device_Refcounted) is
   begin

      LibUSB.Unref_Device (Self.Device);

   end Device_Release;

   procedure Device_List_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Device_List_Refcounted) is
   begin

      Output.Put
        ("Device_List_Refcounted => " &
         System.Address_Image (Value.Devices'Address));

   end Device_List_Put_Image;

   procedure Device_List_Release (Self : in out Device_List_Refcounted) is
   begin

      Log.Debug ("Freeing device list: {0}", Self'Image);
      LibUSB.Free_Device_List (Self.Start, 0);

   end Device_List_Release;

   function Get_Device_List (Ctx : Context'Class) return Device_List is

      package Pointers renames LibUSB.Device_Pointers;

      Rc                : Device_List_Refcounted;
      Number_Of_Devices : C.long;
      Devices           : Device_List;

      use Interfaces.C;

   begin

      Number_Of_Devices := LibUSB.Get_Device_List (Ctx.Get.Context, Rc.Start);

      if Number_Of_Devices < 0 then
         Check_Error (C.int (Number_Of_Devices));
      end if;

      Log.Debug
        ("Got device list {0} with {1} devices", Rc.Start'Image,
         Number_Of_Devices'Image);

      declare

         package C renames Interfaces.C;

         Device_Array : constant LibUSB.Device_Array :=
           Pointers.Value
             (Pointers.Pointer (Rc.Start), C.ptrdiff_t (Number_Of_Devices));
         Dev_Ptr      : Pointers.Pointer := Pointers.Pointer (Rc.Start);
         Dev          : aliased Device;
         Dev_Rc       : Device_Refcounted;

      begin

         for I in Device_Array'Range loop

            Log.Debug ("Got device {0}", Dev_Ptr.all'Image);

            Dev_Rc.Device := Dev_Ptr.all;
            Dev.Set (Dev_Rc);
            Rc.Devices.Append (Dev);

            Pointers.Increment (Dev_Ptr);

         end loop;

      end;

      Devices.Set (Rc);

      return Devices;

   end Get_Device_List;

   overriding function First
     (Object : Device_List_Iterator) return Device_List_Cursor is
     (Device_List_Cursor'(Data => Object.Data.Iterate.First));

   overriding function Next
     (Object   : Device_List_Iterator;
      Position : Device_List_Cursor)
      return Device_List_Cursor is
     (Device_List_Cursor'(Data => Object.Data.Iterate.Next (Position.Data)));

   function Device_List_Has_Element
     (Position : Device_List_Cursor) return Boolean is
     (Device_Vectors.Has_Element (Position.Data));

   function Device_List_Iterate
     (Container : Device_List)
      return Device_List_Iterators.Forward_Iterator'Class is
     (Device_List_Iterator'(Data => Container.Get.Devices'Access));

   function Device_List_Reference
     (Container : aliased Device_List;
      Position  : Device_List_Cursor)
      return Device_List_Constant_Reference is
     (Device_List_Constant_Reference'
        (Element =>
           Container.Get.Devices.Constant_Reference (Position.Data).Element));

   function Get_Device_Descriptor (Dev : Device) return Device_Descriptor is

      function Kind_From_Code is new Ada.Unchecked_Conversion
        (C.unsigned_char, Descriptor_Kind);
      function Class_From_Code is new Ada.Unchecked_Conversion
        (C.unsigned_char, Class);

      Raw_Descriptor : LibUSB.Device_Descriptor;
      Descriptor     : Device_Descriptor;
      Error_Code     : C.int;

   begin

      Error_Code :=
        LibUSB.Get_Device_Descriptor (Dev.Get.Device, Raw_Descriptor);
      Check_Error (Error_Code);

      Descriptor.Length := Natural (Raw_Descriptor.Length);
      Descriptor.Kind   := Kind_From_Code (Raw_Descriptor.Kind);

      if not Descriptor.Kind'Valid then
         raise Other_Error
           with "Invalid descriptor type: " & Raw_Descriptor.Kind'Image;
      end if;

      Descriptor.Release_Number := Natural (Raw_Descriptor.Release_Number);
      Descriptor.Device_Class := Class_From_Code (Raw_Descriptor.Device_Class);

      if not Descriptor.Device_Class'Valid then
         raise Other_Error
           with "Invalid USB class: " & Raw_Descriptor.Device_Class'Image;
      end if;

      Descriptor.Device_Subclass := Natural (Raw_Descriptor.Device_Subclass);
      Descriptor.Device_Protocol := Natural (Raw_Descriptor.Device_Protocol);
      Descriptor.Max_Packet_Size_0        :=
        Natural (Raw_Descriptor.Max_Packet_Size_0);
      Descriptor.Vendor_ID := Natural (Raw_Descriptor.Vendor_ID);
      Descriptor.Product_ID := Natural (Raw_Descriptor.Product_ID);
      Descriptor.Device_Release_Number    :=
        Natural (Raw_Descriptor.Device_Release_Number);
      Descriptor.Manufacturer_Index       :=
        Natural (Raw_Descriptor.Manufacturer_Index);
      Descriptor.Product_Index := Natural (Raw_Descriptor.Product_Index);
      Descriptor.Serial_Number_Index      :=
        Natural (Raw_Descriptor.Serial_Number_Index);
      Descriptor.Number_Of_Configurations :=
        Natural (Raw_Descriptor.Number_Of_Configurations);

      return Descriptor;

   end Get_Device_Descriptor;

end USB;
