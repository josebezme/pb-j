package plt.pbj;

public interface Commands {
	
	String CLOSE = "#CLOSE";
	
	public interface Master {
		String HI = "#HI";
		String JOB = "#JOB";
		String JOB_END = "#JOB_END";
		String ABORT = "#ABORT";
	}
	
	public interface Slave {
		String HI_BACK = "#HI BACK AT YA";
		String REPORT = "#REPORT";
		String REPORT_END = "#REPORT_END";
		String RETURN = "RETURN";
		String RETURN_END = "RETURN_END";
	}
}
