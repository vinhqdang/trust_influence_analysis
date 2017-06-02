package common;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface IGameEventsListener extends Remote {

	void myTurn(double amount) throws RemoteException;

	void firstTurn(boolean yourTurn) throws RemoteException;

	void gameFinished(boolean firstPlayerZero) throws RemoteException;

	void setButtonGame(String gameChoice) throws RemoteException;
}
