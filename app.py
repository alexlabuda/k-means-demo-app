# LIBRARIES ---------------------------------------------------------------

import shiny
from shiny import App, render, reactive, ui
import plotly.express as px
import seaborn as sns
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
import palmerpenguins
import tempfile

# Set default theme for plots
sns.set_theme(style="whitegrid")

# Read data used for the app
pengs_df = palmerpenguins.load_penguins()
iris_df = sns.load_dataset('iris').rename(columns={"species": "Species"})

# Create a list of datasets
datasets = {
    "Penguins": pengs_df.dropna(),
    "Iris": iris_df.dropna()
}

# Select column names for scatter plot x, y-axis selections
names_penguins = pengs_df.select_dtypes(include='number').columns.tolist()
names_iris = iris_df.select_dtypes(include='number').columns.tolist()


# UI ----------------------------------------------------------------------

app_ui = ui.page_fluid(
    ui.h2("K-Means Clustering Demo"),
    ui.layout_sidebar(
        ui.panel_sidebar(
            ui.p("This interactive tool is designed to help you explore the application of the K-means clustering algorithm on different datasets."),
            ui.input_select(
                id="dataset",
                label="Choose a dataset",
                choices=list(datasets.keys()),
                selected="Penguins"
            ),
            ui.input_slider(
                id="k",
                label="Number of clusters",
                min=1,
                max=5,
                value=3,
                step=1
            ),
            ui.panel_conditional(
                "input.dataset == 'Penguins'",
                ui.input_select(
                    id="penguins_x",
                    label="X-axis Column",
                    choices=names_penguins,
                    selected="bill_length_mm"
                ),
                ui.input_select(
                    id="penguins_y",
                    label="Y-axis Column",
                    choices=names_penguins,
                    selected="body_mass_g"
                )
            ),
            ui.panel_conditional(
                "input.dataset == 'Iris'",
                ui.input_select(
                    id="iris_x",
                    label="X-axis Column",
                    choices=names_iris,
                    selected="Sepal.Length"
                ),
                ui.input_select(
                    id="iris_y",
                    label="Y-axis Column",
                    choices=names_iris,
                    selected="Sepal.Width"
                )
            )
        ),
        ui.panel_main(
            ui.output_plot("plot"),
            ui.output_plot("cluster_plot"),
            ui.output_plot("tune_plot")
        )
    )
)


def server(input, output, session):
    
    @reactive.Calc
    def selected_data():
        return datasets[input.dataset()]

    def save_plotly_image(fig):
        temp_file = tempfile.NamedTemporaryFile(delete=False, suffix=".png")
        fig.write_image(temp_file.name)
        return temp_file.name

    @output
    @render.image
    def plot():
        data = selected_data()
        x_col = input.penguins_x() if input.dataset() == "Penguins" else input.iris_x()
        y_col = input.penguins_y() if input.dataset() == "Penguins" else input.iris_y()
        color_col = 'species' if input.dataset() == "Penguins" else 'Species'

        fig = px.scatter(data, x=x_col, y=y_col, color=color_col, title=f"{input.dataset()} Dataset")
        fig.update_layout(legend=dict(
            yanchor="top", y=1.0, xanchor="right", x=1.0
        ))

        return {"src": save_plotly_image(fig)}

    @output
    @render.image
    def cluster_plot():
        data = selected_data().select_dtypes(include='number')

        scaler = StandardScaler()
        scaled_data = scaler.fit_transform(data)
        
        kmeans = KMeans(n_clusters=input.k())
        kmeans.fit(scaled_data)
        
        data['Cluster'] = kmeans.labels_

        centroids = pd.DataFrame(scaler.inverse_transform(kmeans.cluster_centers_), columns=data.columns[:-1])
        x_col = input.penguins_x() if input.dataset() == "Penguins" else input.iris_x()
        y_col = input.penguins_y() if input.dataset() == "Penguins" else input.iris_y()

        fig = px.scatter(data, x=x_col, y=y_col, color='Cluster', title=f"Clustering on {input.dataset()} Dataset")
        fig.add_scatter(x=centroids[x_col], y=centroids[y_col], mode='markers', marker=dict(color='Black', size=15, symbol='square'))
        fig.update_layout(showlegend=False)
        return {"src": save_plotly_image(fig)}

    @output
    @render.image
    def tune_plot():
        data = selected_data().select_dtypes(include='number')

        scaler = StandardScaler()
        scaled_data = scaler.fit_transform(data)

        sse = []
        for k in range(1, 10):
            kmeans = KMeans(n_clusters=k)
            kmeans.fit(scaled_data)
            sse.append(kmeans.inertia_)

        elbow = pd.DataFrame({"Clusters": range(1, 10), "SSE": sse})

        fig = px.line(elbow, x="Clusters", y="SSE", markers=True, title="SSE vs. Number of Clusters")

        return {"src": save_plotly_image(fig)}


app = App(app_ui, server)

# Run the app
if __name__ == "__main__":
    app.run()