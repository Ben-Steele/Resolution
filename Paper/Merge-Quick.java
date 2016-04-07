import java.lang.Math;
import java.util.*;
import java.util.ArrayList;

public class Merge {
    
    long counter;
    long timer;
    

    public int[] merge_quick_sort(int[] a) {
	counter = 0;
	return merge_sort2(a);
    }
    
    public int[] merge_quick_sort2(int[] list_a) {
	long start = System.nanoTime();
	if (list_a.length < 30) {
	    return quick_sort(new ArrayList(list_a));
	}
	int middle = (int)Math.ceil(list_a.length/2.0);
	int[] left = merge_quick_sort2(Arrays.copyOfRange(list_a, 0, middle));
	int[] right = merge_quick_sort2(Arrays.copyOfRange(list_a, middle, list_a.length));
	timer = System.nanoTime() - start;
	return combine(left, right);
    }

    public int[] combine(int[] a, int[] b) {
	int i = 0;
	int j = 0;
	int position = 0;
	int[] nList = new int[a.length+b.length];
	while (position < nList.length) {
	    if(i == a.length) {
		nList[position] = b[j];
		j++;
	    }
	    else if(j == b.length) {
		nList[position] = a[i];
		i++;
	    }
	    else if (a[i] < b[j]) {
		nList[position] = a[i];
		i++;
	    }
	    else {
		nList[position] = b[j];
		j++;
	    }
	    counter++;
	    position++;
	}
	return nList;
    }

        public ArrayList<Integer> quick_sort2( ArrayList<Integer> list_a) {
	long start = System.nanoTime();
	if (list_a.size() == 2) {
	    counter++;
	    if (list_a.get(0) <= list_a.get(1)) {
		return list_a;
	    }
	    else{
		int temp = list_a.get(0);
		list_a.set(0, list_a.get(1));
		list_a.set(1,temp);
		return list_a;
	    }
	}
	else if (list_a.size() == 1) {
	    return list_a;
	}
	ArrayList<Integer> left = new ArrayList();
	ArrayList<Integer> right = new ArrayList();
	for (int i=1;i<list_a.size();i++) {
	    if (list_a.get(0) >= list_a.get(i)) {
		left.add(list_a.get(i));
	    }
	    else {
		right.add(list_a.get(i));
	    }
	    counter++;
	}
	if (left.size() != 0) { 
	    left = quick_sort2(left);
	}
	if (right.size() != 0) {
	    right = quick_sort2(right);
	}
	timer = System.nanoTime() - start;
	return combine(left, list_a.get(0), right);
    }

    public ArrayList<Integer> combine(ArrayList<Integer> left, int pivot, ArrayList<Integer> right) {
	left.add(pivot);
	left.addAll(right);
	return left;
    }
    
    public static void main(String[] args) {
	Merge inn = new Merge();
	int[] w = {5,8,3,4,6,1,9};
	for (int i : inn.merge_sort(w)) {
	    System.out.print(i + " ");
	}
    }
}
