package server.model;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import server.application.UserSession;

import common.DataSample;
import common.FileSaver;

public class Room implements Serializable {

	private static final long serialVersionUID = 1L;
	private static final double CONSTANT = 0.5;

	private Game game;
	private int repetitionNumber;
	private int stroke;
	private int period;
	private ArrayList<UserSession> users;
	private ArrayList<DataSample> usersDataCycle;
	private int capacity;
	private FileSaver fileSaver;

	public Room(Game game, int capacity, int repetitionNumber) {
		this.game = game;
		this.stroke = 0;
		this.period = 0;
		users = new ArrayList<UserSession>();
		usersDataCycle = new ArrayList<DataSample>();
		this.capacity = capacity;
		this.repetitionNumber = repetitionNumber;
		this.fileSaver = new FileSaver();
		if (!this.fileSaver.getSettingsClient().exists()) {
			this.fileSaver.generateCsvClient();
		}
	}

	private void newCycle() {
		DataSample sample = new DataSample();
		usersDataCycle.add(sample);
	}

	public boolean isFull() {
		return users.size() >= capacity;
	}
	
	public boolean contains(UserSession session) {
		return users.contains(session);
	}
	
	public boolean nextTurn() throws ClassNotFoundException, IOException {
		stroke++;
		if (stroke >= repetitionNumber) {
			end(false);
			return false;
		} else {
			return true;
		}
	}

	public void addUser(UserSession userSession) {
		users.add(userSession);
	}

	public void calculatePeriod(int stroke) {
		if (stroke == 0) {
			this.period = 0;
		} else {
			if (stroke % 2 == 0) {
				this.period++;
			}
		}
	}
	
	/*
	 * without initial endowment
	 
	public void initializeEndowment() {
		for(UserSession sess: users) {
			sess.getUser().setMoney(this.game.getEndowment());
		}
	}
	
	*/

	public void end(boolean firstPlayerZero) throws ClassNotFoundException,
			IOException {
		for (UserSession user : users) {
			game.removeUser(user);
			user.fireGameFinished(firstPlayerZero);
		}
		game.removeRoom(this);
		game.save();
	}

	public void play(UserSession session, double amountToPlay, double timeToPlay)
			throws ClassNotFoundException, IOException {

		if (stroke >= repetitionNumber)
			throw new RuntimeException("Game over!");

		if (session != getCurrentUser())
			throw new RuntimeException("Not your turn!");

		User current = session.getUser();
		User opponent = session.getUser();
		
		current.spend(amountToPlay);

		double earning = 0.0;

		for (UserSession sess : users) {
			if (sess != session) {
				if (sess.getRoom().getStroke() == sess.getRoom().getUsers()
						.size() - 1)
					earning = (amountToPlay / (getUsers().size() - 1));
				else
					earning = (amountToPlay / (getUsers().size() - 1)) * 3
							* (sess.getRoom().getStroke() + 1);
				opponent = sess.getUser();
				opponent.earn(earning);
				opponent.setMoneyReceived(earning);
				opponent.setSingleMoneyReceived(amountToPlay);
			}
		}
		calculatePeriod(stroke);

		if (stroke % 2 != 0) {
			opponent.newTrustCalculator(current, CONSTANT, period);
			opponent.setTrustIndexOpponent(current.getTrustIndex());
			current.setTrustIndexOpponent(opponent.getTrustIndex());
		}

		usersDataCycle.get(stroke).setTimeToPlay(timeToPlay);

		if (stroke % capacity == 0) {
			usersDataCycle.get(stroke / capacity).addGivenAmount(amountToPlay);
			usersDataCycle.get(stroke / capacity).setMe(current);
			usersDataCycle.get(stroke / capacity).setPeriod(period+1);
		} else {
			usersDataCycle.get(stroke / capacity).addReceivedAmount(
					amountToPlay);
			usersDataCycle.get(stroke / capacity).setMe(current);
			usersDataCycle.get(stroke / capacity).setOpponent(opponent);
			
			game.getHistogram().getDataSamples()
					.add(usersDataCycle.get(stroke / capacity));
			this.fileSaver.setClientData(usersDataCycle.get(stroke / capacity));
			this.fileSaver.writeCsvClient();
		}

		newCycle();

		if (amountToPlay == 0 && getStroke() == 0) {
			end(true);
			return;
		}

		if (nextTurn()) {
			getCurrentUser().fireTurnEvent(amountToPlay);
		}
	}

	public void startGame() throws ClassNotFoundException, IOException {
		if (game.getTypeAI().getModelAI().equals("Without AI"))
			Collections.shuffle(users);
		newCycle();
		for (UserSession user : users) {
			user.fireFirstTurn(user == getCurrentUser());
			
			//without initial endowment
			//user.getUser().setMoney(this.game.getEndowment());
		}
	}

	public List<UserSession> getUsers() {
		return users;
	}

	public int getRepetitionNumber() {
		return this.repetitionNumber;
	}

	public int getStroke() {
		return (stroke % users.size());
	}

	public UserSession getCurrentUser() {
		return users.get(stroke % users.size());
	}

	public int getPeriod() {
		return period;
	}

	public void setPeriod(int period) {
		this.period = period;
	}
}