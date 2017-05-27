with Dirent; use Dirent;
with System.Multiprocessors; use System.Multiprocessors;
with GNAT.Regexp;
with Stack;

generic
	type CPU_Local is range <>;
	Max_Depth : Natural;
	Match_Token : GNAT.Regexp.Regexp;
	Match_All : Boolean;
	with procedure Write (Directory : in String; Mode : in Directory_Mode);

package Finder is
	pragma Elaborate_Body;
	pragma Suppress (All_Checks);

	procedure Find_Start (Directory : in String);

private

	task type Thread (Num : CPU_Local) with CPU => CPU(Num) is
		entry Run (D : in String; Depth : in Natural);
	end Thread;

	type Thread_Access is access all Thread;
	type Thread_Access_Array is array (CPU_Local) of Thread_Access;

	package Thread_Stack is new Stack (Positive(CPU_Local'Last), CPU_Local);

	protected Task_Pool is
		procedure Initialize;
		procedure Check_Out (Thread_Pointer : out Thread_Access);
		procedure End_Thread (Thread_Num : in CPU_Local);
	private
		Status : Thread_Stack.Stack_Type;
		Threads : Thread_Access_Array;
		Is_Initialized : Boolean := False;
	end Task_Pool;

	procedure Find (
		Directory : in String;
		Depth : in Natural);

end Finder;