package common;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import server.model.Game;

@SuppressWarnings("serial")
public class FileSaver implements Serializable {

	private static final File settingsServer = new File("settings_server.csv");
	private static final File settingsClient = new File("settings_client.csv");

	private Game game;
	private DataSample clientData;

	public FileSaver() {
		this.game = new Game();
		this.clientData = new DataSample();
	}

	public File getSettingsServer() {
		return settingsServer;
	}

	public File getSettingsClient() {
		return settingsClient;
	}

	public void setGame(Game game) {
		this.game = game;
	}

	public void setClientData(DataSample clientData) {
		this.clientData = clientData;
	}

	public void generateCsvServer() {
		try {
			settingsServer.createNewFile();
			FileWriter writer = new FileWriter(settingsServer.getName(), true);
			writer.append("Date");
			writer.append(',');
			writer.append("Number of periods");
			writer.append(',');
			writer.append("Amount per user");
			writer.append(',');
			writer.append("Game type choice");
			writer.append(',');
			writer.append("AI choice");
			writer.append('\n');
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void generateCsvClient() {
		try {
			settingsClient.createNewFile();
			FileWriter writer = new FileWriter(settingsClient.getName(), true);
			writer.append("Date");
			writer.append(',');
			writer.append("Period");
			writer.append(',');
			writer.append("User");
			writer.append(',');
			writer.append("Opponent");
			writer.append(',');
			writer.append("Amount sent");
			writer.append(',');
			writer.append("Amount received");
			writer.append(',');
			writer.append("Time");
			writer.append(',');
			writer.append("Balance User");
			writer.append(',');
			writer.append("Balance Opponent");
			writer.append(',');
			writer.append("Trust index User");
			writer.append(',');
			writer.append("Trust index Opponent");
			writer.append('\n');
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void writeCsvServer() {
		try {
			FileWriter writer = new FileWriter(settingsServer.getName(), true);
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy hh:mm a");
			Date date = new Date();
			writer.append(dateFormat.format(date));
			writer.append(',');
			writer.append(this.game.getAllowedStrokes() + "");
			writer.append(',');
			writer.append(this.game.getAmountPerUser() + "");
			writer.append(',');
			writer.append(this.game.getGameChoice() + "");
			writer.append(',');
			writer.append(this.game.getTypeAI().getModelAI());
			writer.append('\n');
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void writeCsvClient() {
		try {
			FileWriter writer = new FileWriter(settingsClient.getName(), true);
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy hh:mm a");
			Date date = new Date();
			writer.append(dateFormat.format(date));
			writer.append(',');
			writer.append(this.clientData.getPeriod() + "");
			writer.append(',');
			writer.append(this.clientData.getMe().getId());
			writer.append(',');
			writer.append(this.clientData.getOpponent().getId());
			writer.append(',');
			writer.append(this.clientData.getReceivedAmount() + "");
			writer.append(',');
			writer.append(this.clientData.getGivenAmount() + "");
			writer.append(',');
			writer.append(this.clientData.getTimeToPlay() + "");
			writer.append(',');
			writer.append(this.clientData.getMe().getMoney() + "");
			writer.append(',');
			writer.append(this.clientData.getOpponent().getMoney() + "");
			writer.append(',');
			writer.append(this.clientData.getMe().getTrustIndex() + "");
			writer.append(',');
			writer.append(this.clientData.getOpponent().getTrustIndex() + "");
			writer.append('\n');
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}