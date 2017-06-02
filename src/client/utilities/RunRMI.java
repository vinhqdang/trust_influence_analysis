package client.utilities;

import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import common.IServerRMI;

public class RunRMI {

	private IServerRMI stub;
	private Registry registry;

	public RunRMI() {
		try {
			registry = LocateRegistry.getRegistry(10000);
			stub = (IServerRMI) registry.lookup("Add");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public IServerRMI getStub() {
		return stub;
	}

	public void setStub(IServerRMI stub) {
		this.stub = stub;
	}

}
