with GNAT.Regexp;

generic
	type CPU is range <>;

package Finder is

	type Search_Mode is (
		Plain,
		Regular_Expression);

	procedure Find_Start (
		Directory, Token : in String;
		Desired_Depth : in Natural := Natural'Last);

private

	Max_Depth : Natural;

	Match_Token : GNAT.Regexp.Regexp;

	task type Thread (Num : CPU) is
		entry Run (D : in String; Depth : in Natural);
	end Thread;

	type Thread_Access is access all Thread;
	type Thread_Access_Array is array (CPU) of Thread_Access;
	type Status is (Working, Ready);
	type Status_Array is array (CPU) of Status;

	Threads : Thread_Access_Array := (others=>Null);

	protected Task_Pool_Status is
		procedure Check (Thread_Pointer : out Thread_Access);
		procedure End_Thread (Thread_Num : in CPU);
	private
		Status : Status_Array := (others=>Ready);
	end Task_Pool_Status;

	procedure Find (
		Directory : in String;
		Depth : in Natural);

	procedure Write (Directory : in String);

end Finder;