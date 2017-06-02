package client.application;

import java.io.IOException;
import java.util.List;

import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Point2D;
import javafx.scene.Node;
import javafx.scene.chart.BarChart;
import javafx.scene.chart.CategoryAxis;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseEvent;

import common.DataSample;
import common.Histogram;

public class HistogramView extends BarChart<String, Number> {

	private CategoryAxis xAxis;
	private NumberAxis yAxis;
	private XYChart.Series<String, Number> spentSeries;
	private XYChart.Series<String, Number> earnedSeries;
	private XYChart.Series<String, Number> tripleSeries;

	public HistogramView() {
		super(new CategoryAxis(), new NumberAxis());
		this.xAxis = (CategoryAxis) getXAxis();
		this.yAxis = (NumberAxis) getYAxis();

		FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource(
				"HistogramView.fxml"));
		fxmlLoader.setRoot(this);
		fxmlLoader.setController(this);

		try {
			fxmlLoader.load();
		} catch (IOException exception) {
			throw new RuntimeException(exception);
		}
	}

	@SuppressWarnings("unchecked")
	public void initialize() {
		setTitle("Game Histogram");
		xAxis.setLabel("Periods");
		yAxis.setLabel("Amount");

		spentSeries = new Series<>();
		spentSeries.setName("Spent");

		earnedSeries = new Series<>();
		earnedSeries.setName("Received");

		tripleSeries = new Series<>();
		tripleSeries.setName("3 * Spent");

		getData().addAll(spentSeries, earnedSeries, tripleSeries);
	}

	public void setHistogram(Histogram histogram, List<String> userList) {
		setVisible(histogram != null);

		if (histogram == null)
			return;

		DataSample[] samples = histogram.getDataSamples().toArray(
				new DataSample[0]);
		for (int i = samples.length - 1; i >= 0; i--) {
			DataSample sample = samples[i];
			spentSeries.getData().add(
					new Data<String, Number>("Period " + i, sample
							.getGivenAmount()));
			earnedSeries.getData().add(
					new Data<String, Number>("Period " + i, sample
							.getReceivedAmount()));
			tripleSeries.getData().add(
					new Data<String, Number>("Period " + i, 3 * sample
							.getGivenAmount()));
		}

		for (Series<String, Number> s : getData()) {
			for (final Data<String, Number> data : s.getData()) {
				final Tooltip tooltip = new Tooltip(data.getYValue() + "");
				final Node node = data.getNode();
				node.setOnMouseEntered(new EventHandler<MouseEvent>() {

					public void handle(MouseEvent event) {
						Point2D p = node.localToScene(node.getLayoutBounds()
								.getMaxX(), node.getLayoutBounds().getMaxY());
						tooltip.show(node, p.getX()
								+ node.getScene().getWindow().getX(), p.getY()
								+ node.getScene().getWindow().getY());
					}
				});
				node.setOnMouseExited(new EventHandler<MouseEvent>() {

					@Override
					public void handle(MouseEvent event) {
						tooltip.hide();
					}
				});
				Tooltip.install(node, tooltip);
			}
		}
	}
}
