package common;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface IServerRMI extends Remote {
	IUserSession login(String mail, String password,
			IGameEventsListener listener) throws RemoteException;

	Histogram getHistogram() throws RemoteException;
}