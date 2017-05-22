with Dirent; use Dirent;
with GNAT.Regexp;
with Stack;


generic
	type CPU is range <>;
	Max_Depth : Natural;
	Match_Token : GNAT.Regexp.Regexp;
	Pretty_Print : Boolean;

package Finder is
	pragma Elaborate_Body;

	procedure Find_Start (Directory : in String);

private

	task type Thread (Num : CPU) is
		entry Run (D : in String; Depth : in Natural);
	end Thread;

	type Thread_Access is access all Thread;
	type Thread_Access_Array is array (CPU) of Thread_Access;

	package Thread_Stack is new Stack (Positive(CPU'Last), CPU);

	protected Task_Pool is
		procedure Initialize;
		procedure Check_Out (Thread_Pointer : out Thread_Access);
		procedure End_Thread (Thread_Num : in CPU);
	private
		Status : Thread_Stack.Stack_Type;
		Threads : Thread_Access_Array;
		Is_Initialized : Boolean := False;
	end Task_Pool;

	procedure Find (
		Directory : in String;
		Depth : in Natural);

	procedure Write (Directory : in String; Mode : in Directory_Mode);

end Finder;