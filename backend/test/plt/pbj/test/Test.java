package plt.pbj.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;

public class Test {
	
	
	public static void main(String[] args) {
		Double d = 0.0;
		
		Long l = 0l;
		
		Map<Object, Object> objects = new HashMap<Object, Object>();
		
		objects.put("Long", l);
		objects.put("d", d);
		
		Gson gson = new Gson();
		
		String json = gson.toJson(objects);
		System.out.println("gson: " + json);
		
//		objects = gson.fromJson(json, classOfT)
		
		
		
		
		
		
	}
}
