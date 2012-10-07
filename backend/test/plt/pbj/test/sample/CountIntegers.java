package plt.pbj.test.sample;

import com.google.gson.Gson;

public class CountIntegers {
	
	private static final Gson gson = new Gson();
	
	public static String countIntegers(String input) {
		
		Integer[] integers = gson.fromJson(input, Integer[].class); 
		
		Integer result = 0;
		
		for(Integer num : integers) {
			result += num;
		}
		
		return "" + result;
	}

}
