package plt.pbj.util;

import java.util.HashMap;
import java.util.Map;

public class MapUtil {
	
	public static Map<Object, Object> toMap(Object[] objects) {
		Map<Object, Object> map = new HashMap<Object, Object>();
		
		for(int i = 0; i < objects.length; i = i + 2) {
			map.put(objects[i], objects[i + 1]);
		}
		
		return map;
	}

}
