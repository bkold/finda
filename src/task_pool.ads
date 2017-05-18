with System.Multiprocessors; use System.Multiprocessors;
generic
	with procedure Worker (Directory, Token : in String);
	with procedure Writer (Directory : in String);
package Task_Pool is

	task type Thread is
		entry Run (Directory, Token : in String);
	end Thread;

	type Task_Array is array (CPU) of Thread;

	type Tasks_Status_Array_Type is array (CPU) of Boolean;

	protected type Task_Pool_Status is
		function Has_Empty return CPU_Range;
		procedure Free (Task_Number : in CPU);
	private
		Tasks_Status_Array : Tasks_Status_Array_Type := (others=>False);
	end Task_Pool_Status;

	type Task_Object is record
		Tasks : Task_Array;
		Status : Task_Pool_Status;
	end record;

end Task_Pool;