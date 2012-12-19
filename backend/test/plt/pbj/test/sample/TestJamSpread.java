package plt.pbj.test.sample;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;

import plt.pbj.PBJ;
import plt.pbj.Spreadable;
import plt.pbj.master.SlaveHandler;
import plt.pbj.util.PBJOp;

public class TestJamSpread {

	private static final Gson gson = PBJ.gson;
	
	public static void main(String[] args) {
		testSlice();
		testJson();
	}
	
	private static void testJson() {
		List<Object> list = new ArrayList<Object>();
		list.add(new Long(1));
		
		Object[] objs = new Object[]{list};
		
		String json = gson.toJson(objs);
		
		System.out.println("Json: " + json);
		
		objs = gson.fromJson(json, Object[].class);
		
		objs = PBJOp.cleanUpArray(objs);
		
		System.out.println("objs: " + Arrays.toString(objs));
		System.out.println("obj type: " + ((List<Object>)objs[0]).get(0).getClass().getSimpleName());
		
	}

	private static void testSlice() {
		List<SlaveHandler> handlers = Arrays.asList(
				new SlaveHandler[] {new SlaveHandler("sone"), new SlaveHandler("stwo")});
		
		
		
		Map<Object, Object> map = new HashMap<Object, Object>();
		map.put("long-1", new Long(1));
		map.put("double-2", new Double(2.0));
		map.put("yada", "Yada-val");
		
		Object[] objs = new Object[] {
				new Spreadable(map), "Can't touch this."
		};
		
		Map<SlaveHandler, Object[]> slicedMap = PBJOp.slice(objs, handlers);
		
		System.out.println("slicedMap:");
		for(SlaveHandler handler : slicedMap.keySet()) {
			System.out.println(handler + ":" + Arrays.toString(slicedMap.get(handler)));
			System.out.println(gson.toJson(slicedMap.get(handler)));
		}
		
		List<Object> list = new ArrayList<Object>();
		
		for(int i = 0; i < 13; i++) {
			list.add(new Long(i));
		}
		
		objs = new Object[]{ new Spreadable(list), "Don't touch this"};
		
		slicedMap = PBJOp.slice(objs, handlers);
		
		System.out.println("slicedList:");
		for(SlaveHandler handler : slicedMap.keySet()) {
			System.out.println(handler + ":" + Arrays.toString(slicedMap.get(handler)));
			
			System.out.println(gson.toJson(slicedMap.get(handler)));
		}
		
		
		list = new ArrayList<Object>();
		list.add("args");
		objs = new Object[]{new Spreadable(list)};
		
		PBJOp.slice(objs, handlers);

	}

}
