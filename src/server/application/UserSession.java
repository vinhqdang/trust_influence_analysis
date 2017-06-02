package server.application;

import java.io.IOException;
import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

import server.model.Game;
import server.model.Room;
import server.model.User;
import common.IGameEventsListener;
import common.IUserSession;

public class UserSession implements IUserSession, Serializable {

	private static final long serialVersionUID = 1L;

	private User user;
	private Game game;
	private Room room;

	private IGameEventsListener listener;
	private String gameChoice;

	public UserSession(Game game, User user, IGameEventsListener listener)
			throws RemoteException {
		this.user = user;
		this.game = game;
		this.listener = listener;
	}

	public User getUser() {
		return user;
	}

	public Game getGame() {
		return game;
	}

	public List<String> getUsers() {
		List<String> listName = new ArrayList<String>();
		if (room != null) {
			for (UserSession userSession : room.getUsers()) {
				listName.add(userSession.getUser().getId());
			}
		}
		return listName;
	}

	public Room getRoom() {
		return room;
	}

	public void setRoom(Room room) {
		this.room = room;
	}

	@Override
	public void play(double amountToPlay, double timeToPlay)
			throws ClassNotFoundException, IOException {
		room.play(this, amountToPlay, timeToPlay);
	}

	@Override
	public void logout() throws ClassNotFoundException, IOException {
		game.userLogout(this);
	}

	@Override
	public void joinRoom() throws ClassNotFoundException, IOException {
		this.room = game.assignUserRoom(this);
	}

	public void fireTurnEvent(double amountToPlay)
			throws ClassNotFoundException, IOException {
		try {
			listener.myTurn(amountToPlay);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
	}

	public void fireGameFinished(boolean firstPlayerZero) throws IOException,
			ClassNotFoundException {
		try {
			listener.gameFinished(firstPlayerZero);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
	}

	public void fireFirstTurn(boolean yourTurn) throws ClassNotFoundException,
			IOException {
		try {
			listener.firstTurn(yourTurn);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
	}

	public void setButtonGame(String gameChoice) {
		this.gameChoice = gameChoice;
	}

	public String getGameChoice() throws RemoteException {
		return gameChoice;
	}

	@Override
	public void setMoneyUser(double n) throws RemoteException {
		this.user.setMoney(n);
	}
	
	@Override
	public double getMoneyUser() throws RemoteException {
		return user.getMoney();
	}

	@Override
	public void setMoneyReceived(double n) throws RemoteException {
		this.user.setMoneyReceived(n);
	}

	@Override
	public double getMoneyReceived() throws RemoteException {
		return user.getMoneyReceived();
	}

	@Override
	public void setTrustIndex(double n) throws RemoteException {
		this.user.setTrustIndex(n);
	}

	@Override
	public double getTrustIndex() throws RemoteException {
		return this.user.getTrustIndex();
	}

	@Override
	public void setTrustIndexOpponent(double n) throws RemoteException {
		this.user.setTrustIndexOpponent(n);
	}

	@Override
	public double getTrustIndexOpponent() throws RemoteException {
		return this.user.getTrustIndexOpponent();
	}

}
