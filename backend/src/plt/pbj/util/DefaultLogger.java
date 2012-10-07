package plt.pbj.util;

public class DefaultLogger implements Logger {
	
	private static final Logger logger = new DefaultLogger();
	
	public static Logger getDefaultLogger() {
		return logger;
	}
	
	private DefaultLogger() {
	}

	@Override
	public void log(String message) {
		System.out.println(message);
	}

}
