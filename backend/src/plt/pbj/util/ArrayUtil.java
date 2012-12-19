package plt.pbj.util;

import java.util.List;

public class ArrayUtil {
	
	public static void set(List<Object> list, Long idx, Object e) {
		if(list.size() >= idx) {
			list.add(idx.intValue(), e);
		} else {
			list.set(idx.intValue(), e);
		}
	}

}
