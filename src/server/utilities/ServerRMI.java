package server.utilities;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import server.application.UserSession;
import server.model.Game;
import server.model.User;

import common.Histogram;
import common.IGameEventsListener;
import common.IServerRMI;
import common.IUserSession;

public class ServerRMI implements IServerRMI {

	private Game game;
	private static final double START_TRUST_INDEX=1;

	public ServerRMI() throws RemoteException {
	}

	public Game getGame() {
		return game;
	}

	public void setGame(Game game) throws RemoteException {
		this.game = game;
	}

	@Override
	public IUserSession login(String mail, String password,
			IGameEventsListener listener) throws RemoteException {
		User user = game.findUser(mail);

		if (user == null) {
			user = new User(mail, password, game.getAmountPerUser(),START_TRUST_INDEX,0);
		} else if (!user.getPassword().equals(password)) {
			return null;
		}

		UserSession session = new UserSession(game, user, listener);
		game.addUser(session);

		return (IUserSession) UnicastRemoteObject.exportObject(session, 0);
	}

	@Override
	public Histogram getHistogram() throws RemoteException {
		Histogram histo = game.getHistogram();
		return histo;
	}
}
