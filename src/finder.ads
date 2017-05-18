with Dirent;
with GNAT.Regexp;
-- with Task_Pool;

package Finder is

	procedure Initiate (Directory, Query : in String);

	type Search_Mode is (
		Plain,
		Regular_Expression);

	procedure Find_Start (Directory, Token : in String; Desired_Mode : in Search_Mode);

	Dir_Error : Exception;

private
	use Dirent;

	Run_Mode : Search_Mode := Plain;
	Search_Pattern : GNAT.Regexp.Regexp;
	procedure Find (Directory, Token : in String);
	procedure Write (Directory : in String);
	-- package Query_Tasks is new Task_Pool (Worker=>Find, Writer=>Write);
	-- use Query_Tasks;


end Finder;