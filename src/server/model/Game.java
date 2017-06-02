package server.model;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.ArrayList;

import server.application.UserSession;

import common.Histogram;

public class Game implements Serializable {

	private static final long serialVersionUID = 1L;
	private static final int USERS_PER_ROOM = 2;

	private ArrayList<UserSession> userSessionList;
	private ArrayList<User> userGlobalList;
	private ArrayList<Room> roomList;
	private Room currentRoom;
	private Histogram histogram;
	private int usersPerRoom = USERS_PER_ROOM;
	private int allowedStrokes;
	private double amountPerUser;
	
	//without cumulative endowment
	//private double endowment;
	
	private String gameChoice;
	private int nbAIPerRoom;
	private TypeAI typeAI;

	public Game(int usersPerRoom, int allowedStrokes, double amountPerUser,
			String gameChoice, int nbAIPerRoom, TypeAI typeAI)
			throws RemoteException {
		this.usersPerRoom = usersPerRoom;
		this.allowedStrokes = allowedStrokes;
		this.currentRoom = new Room(this, usersPerRoom, allowedStrokes);
		this.userGlobalList = new ArrayList<User>();
		this.userSessionList = new ArrayList<UserSession>();
		this.histogram = new Histogram();
		this.roomList = new ArrayList<Room>();
		this.roomList.add(this.currentRoom);
		this.amountPerUser = amountPerUser;
		this.gameChoice = gameChoice;
		this.nbAIPerRoom = nbAIPerRoom;
		this.typeAI = typeAI;
		
		//without cumulative endowment
		//this.endowment = 0;

		addAI();
	}

	public Game() {
		this.usersPerRoom = 0;
		this.allowedStrokes = 0;
		this.amountPerUser = 0;
		this.gameChoice = "";
		this.nbAIPerRoom = 0;
	}

	public TypeAI getTypeAI() {
		return typeAI;
	}

	public Room getCurrentRoom() {
		return currentRoom;
	}

	public double getAmountPerUser() {
		return amountPerUser;
	}

	public ArrayList<User> getUserGlobalList() {
		return userGlobalList;
	}

	public Histogram getHistogram() {
		return histogram;
	}

	public int getUsersPerRoom() {
		return usersPerRoom;
	}

	public int getAllowedStrokes() {
		return allowedStrokes;
	}

	public String getGameChoice() {
		return gameChoice;
	}

	
	/*
	 * without cumulative endowment
	 
	public double getEndowment() {
		return endowment;
	}

	public void setEndowment(double endowment) {
		this.endowment = endowment;
	}
	
	*/

	public void setUserGlobalList(ArrayList<User> userGlobalList) {
		this.userGlobalList = userGlobalList;
	}

	public void setHistogram(Histogram histogram) {
		this.histogram = histogram;
	}

	public String userGlobalListToString() {
		String chain = "";
		for (int i = 0; i < this.userGlobalList.size(); i++) {
			chain += this.userGlobalList.get(i).userToString();
		}
		return chain;
	}

	public User findUser(String mail) throws RemoteException {
		for (User user : userGlobalList) {
			if (user.getId().equals(mail))
				return user;
		}
		return null;
	}

	public void addUser(UserSession userSession) {
		if (!userSessionList.contains(userSession)) {
			userSession.setButtonGame(this.gameChoice);
			userSessionList.add(userSession);
		}
		if (!userGlobalList.contains(userSession.getUser())) {
			userGlobalList.add(userSession.getUser());
		}
	}

	private Room findUserRoom(UserSession session) {
		for (Room room : roomList) {
			if (room.contains(session))
				return room;
		}
		return null;
	}

	public void removeUser(UserSession user) {
		userSessionList.remove(user);
	}

	public void removeRoom(Room room) {
		roomList.remove(room);
	}

	public ArrayList<Room> getRoomList() {
		return roomList;
	}

	public void userLogout(UserSession userSession)
			throws ClassNotFoundException, IOException {
		Room room = findUserRoom(userSession);

		if (room != null)
			room.end(false);

		userSessionList.remove(userSession);
	}

	public void addAI() throws RemoteException {
		if (!typeAI.getModelAI().equals("Without AI")) {
			for (int i = 0; i < nbAIPerRoom; i++) {
				UserAI userAI = new UserAI("" + i, this.amountPerUser, typeAI);
				UserSession userSessionAI = new UserSession(this, userAI,
						userAI.getAiListener());
				userAI.setSessionAI(userSessionAI);
				userSessionAI.setRoom(currentRoom);
				currentRoom.addUser(userSessionAI);
			}
		}
	}

	public Room assignUserRoom(UserSession userSession)
			throws ClassNotFoundException, IOException {
		Room assignedRoom = currentRoom;

		if (currentRoom.getUsers().contains(userSession)) {
			return assignedRoom;
		}

		userSession.setRoom(currentRoom);
		currentRoom.addUser(userSession);

		if (currentRoom.isFull()) {
			currentRoom.startGame();
			currentRoom = new Room(this, usersPerRoom, allowedStrokes);
			roomList.add(currentRoom);
			addAI();
		}

		return assignedRoom;
	}

	public void save() throws IOException {
		String fileName = "game.bin";
		ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(
				fileName));
		try {
			oos.writeObject(this);
		} catch (IOException ioe) {
			System.err.println("FATAL ERROR -- " + ioe.toString());
		}
		oos.close();
	}

	public Game load() throws IOException {
		String fileName = "game.bin";
		Game tmp = null;
		ObjectInputStream ois = new ObjectInputStream(new FileInputStream(
				fileName));
		try {
			tmp = (Game) ois.readObject();
		} catch (IOException ioe) {
			System.err.println("ERROR -- " + ioe.toString());
		} catch (ClassNotFoundException cnfe) {
			System.err.println("ERROR 'Unknown class' -- " + cnfe.toString());
		}
		ois.close();
		return tmp;
	}

}
