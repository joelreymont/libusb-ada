pragma Ada_2022;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with System;
with GNATCOLL.Refcount;
with Ada.Strings.Text_Buffers;
with LibUSB;

package USB is

  Input_Output_Error, Invalid_Parameter_Error, Access_Denied_Error,
  No_Device_Error, Not_Found_Error, Resource_Busy_Error, Timeout_Error,
  Overflow_Error, Pipe_Error, Syscall_Interrupted_Error, Out_Of_Memory_Error,
  Operation_Not_Supported_Error, Other_Error : exception;

  type Context is tagged private;

  procedure Init;

  function Make_Context return Context;

  type Device is tagged private;
  type Device_List is tagged private;

  function Get_Device_List (Ctx : Context'Class) return Device_List;

private

  type Context_Data is new GNATCOLL.Refcount.Refcounted with record
    Context : LibUSB.Context;
  end record with
   Put_Image => Context_Data_Put_Image;

  procedure Context_Data_Put_Image
   (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
    Value  : Context_Data);

  procedure Context_Release (Self : in out Context_Data);

  package Context_Pointers is new GNATCOLL.Refcount.Shared_Pointers
   (Element_Type => Context_Data,
    Release      => Context_Release);

  type Context is new Context_Pointers.Ref with null record;

  type Device_Data is new GNATCOLL.Refcount.Refcounted with record
    Device : LibUSB.Device;
  end record with
   Put_Image => Device_Data_Put_Image;

  procedure Device_Data_Put_Image
   (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
    Value  : Device_Data);

  procedure Device_Release (Self : in out Device_Data);

  package Device_Pointers is new GNATCOLL.Refcount.Shared_Pointers
   (Element_Type => Device_Data,
    Release      => Device_Release);

  type Device is new Device_Pointers.Ref with null record;

  type Device_List_Data is new GNATCOLL.Refcount.Refcounted with record
    Device_List : LibUSB.Device_List;
    N: Integer;
  end record with
   Put_Image => Device_List_Data_Put_Image;

  procedure Device_List_Data_Put_Image
   (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
    Value  : Device_List_Data);

  procedure Device_List_Release (Self : in out Device_List_Data);

  package Device_List_Pointers is new GNATCOLL.Refcount.Shared_Pointers
   (Element_Type => Device_List_Data,
    Release      => Device_List_Release);

  type Device_List is new Device_List_Pointers.Ref with null record;

end USB;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
