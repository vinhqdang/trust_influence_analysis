package common;

import java.io.Serializable;
import java.util.ArrayList;

public class DataGroup implements Serializable {
	private static final long serialVersionUID = 1L;

	public double key;
	public ArrayList<DataSample> data = new ArrayList<DataSample>();

	public DataGroup() {
	}

	public DataGroup(double key) {
		this.key = key;
	}
}