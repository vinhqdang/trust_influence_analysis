package common;

import java.io.IOException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

public interface IUserSession extends Remote {

	void logout() throws RemoteException, ClassNotFoundException, IOException;

	void play(double amountToPlay, double timeToPlay) throws RemoteException,
			ClassNotFoundException, IOException;

	void joinRoom() throws RemoteException, ClassNotFoundException, IOException;

	void setMoneyUser(double n) throws RemoteException;

	double getMoneyUser() throws RemoteException;
	
	void setMoneyReceived(double n) throws RemoteException;
	
	double getMoneyReceived() throws RemoteException;
	
	void setTrustIndex(double n) throws RemoteException;
	
	double getTrustIndex() throws RemoteException;
	
	void setTrustIndexOpponent(double n) throws RemoteException;
	
	double getTrustIndexOpponent() throws RemoteException;

	String getGameChoice() throws RemoteException;

	List<String> getUsers() throws RemoteException;
}
