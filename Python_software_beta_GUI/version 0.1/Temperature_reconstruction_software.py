import tkinter as tk
from tkinter import filedialog, simpledialog, messagebox
import pandas as pd
import numpy as np
import skfda
import sys

np.float_ = np.float64

def load_csv():
    root = tk.Tk()
    root.withdraw()

    file_path = filedialog.askopenfilename(filetypes=[("CSV files", "*.csv")])
    if file_path:
        try:
            data = pd.read_csv(file_path)
            return data
        except Exception as e:
            messagebox.showerror("Błąd", f"Nie można wczytać pliku: {e}")
            root.destroy()
    else:
        messagebox.showwarning("Ostrzeżenie", "Nie wybrano pliku!")
        root.destroy()

def transformation_of_data(temp_df, n):
    temp_df['date'] = pd.to_datetime(temp_df['date'])

    start_date = min(temp_df['date'])

    temp_df['rep'] = (temp_df['date'] - start_date).dt.days + 1
    temp_df['hour'] = temp_df['date'].dt.hour
    temp_df = temp_df.groupby(['rep', 'hour']).agg({'Y': 'mean', 'X': 'mean'}).reset_index()
    group_counts = temp_df.groupby('rep').size()
    valid_groups = group_counts[group_counts == 24].index
    df_filtered = temp_df[temp_df['rep'].isin(valid_groups)].copy()
    df_filtered['idx'] = df_filtered.groupby('rep').cumcount()  # + 1
    temp = df_filtered.pivot(index='idx', columns='rep', values='Y')
    weather = df_filtered.pivot(index='idx', columns='rep', values='X')

    all_sets = {'weather': weather, 'temp': temp}
    train_sets = {'weather': weather.iloc[:, n:], 'temp': temp.iloc[:, n:]}
    test_sets = {'weather': weather.iloc[:, :n], 'temp': temp.iloc[:, :n]}
    return all_sets, train_sets, test_sets, start_date

def transformation_of_data_2(temp_df):
    temp_df['date'] = pd.to_datetime(temp_df['date'])

    start_date = min(temp_df['date'])

    temp_df['rep'] = (temp_df['date'] - start_date).dt.days + 1
    temp_df['hour'] = temp_df['date'].dt.hour
    temp_df = temp_df.groupby(['rep', 'hour']).agg({'X': 'mean'}).reset_index()
    group_counts = temp_df.groupby('rep').size()
    valid_groups = group_counts[group_counts == 24].index
    df_filtered = temp_df[temp_df['rep'].isin(valid_groups)].copy()
    df_filtered['idx'] = df_filtered.groupby('rep').cumcount()  # + 1
    weather = df_filtered.pivot(index='idx', columns='rep', values='X')

    results = {'weather': weather}
    return results, start_date

def reconstruction_mid_term(train_set):
    t = np.linspace(0, 23, 24)
    fourier_basis = skfda.representation.basis.FourierBasis(domain_range=(0, 23), n_basis=3, period=24)

    Y_train = skfda.FDataGrid(
        data_matrix=train_set['temp'].T,
        grid_points=t,
    ).to_basis(fourier_basis)
    X_train = skfda.FDataGrid(
        data_matrix=train_set['weather'].T,
        grid_points=t,
    ).to_basis(fourier_basis)

    linear_reg = skfda.ml.regression.LinearRegression(
        coef_basis=[fourier_basis])

    _ = linear_reg.fit(X_train, Y_train)

    return linear_reg

def performance_mid_term(test_set, fd_linear_reg):
    t = np.linspace(0, 23, 24)
    fourier_basis = skfda.representation.basis.FourierBasis(domain_range=(0, 23), n_basis=3, period=24)
    X_test = skfda.FDataGrid(
        data_matrix=test_set['weather'].T,
        grid_points=t,
    ).to_basis(fourier_basis)
    Y_test = skfda.FDataGrid(
        data_matrix=test_set['temp'].T,
        grid_points=t,
    ).to_basis(fourier_basis)
    error = np.mean(np.abs(np.squeeze(fd_linear_reg.predict(X_test)(t)) - np.squeeze(Y_test(t))))
    return error


def evaluation_mid_term(all_sets, start_date):
    t = np.linspace(0, 23, 24)
    fourier_basis = skfda.representation.basis.FourierBasis(domain_range=(0, 23), n_basis=3, period=24)

    Y_train = skfda.FDataGrid(
        data_matrix=all_sets['temp'].T,
        grid_points=t,
    ).to_basis(fourier_basis)
    X_train = skfda.FDataGrid(
        data_matrix=all_sets['weather'].T,
        grid_points=t,
    ).to_basis(fourier_basis)

    linear_reg = skfda.ml.regression.LinearRegression(
        coef_basis=[fourier_basis])

    _ = linear_reg.fit(X_train, Y_train)

    eval = np.squeeze(linear_reg.predict(X_train)(t)).T
    eval_Y_train = np.squeeze(Y_train(t)).T

    vector_recon = eval.flatten(order='F')
    vector_local = eval_Y_train.flatten(order='F')
    vector_weather = all_sets['weather'].to_numpy().flatten(order='F')
    dates = pd.date_range(start=start_date, periods=len(vector_recon), freq='h')

    return pd.DataFrame({'date': dates, 'temp_recon': vector_recon, 'temp_local': vector_local, 'temp_weather': vector_weather})

def evaluation_mid_term_2(linear_reg, X):
    t = np.linspace(0, 23, 24)
    fourier_basis = skfda.representation.basis.FourierBasis(domain_range=(0, 23), n_basis=3, period=24)

    results, start_date = transformation_of_data_2(X)

    X_fda = skfda.FDataGrid(
        data_matrix=results['weather'].T,
        grid_points=t,
    ).to_basis(fourier_basis)

    eval = np.squeeze(linear_reg.predict(X_fda)(t)).T

    vector_recon = eval.flatten(order='F')
    dates = pd.date_range(start=start_date, periods=len(vector_recon), freq='h')

    return pd.DataFrame({'date': dates, 'temp_recon': vector_recon})

def main():
    root = tk.Tk()
    root.withdraw()
    messagebox.showinfo("Warning!", "Currently, only Mid-term model is supported. Please remember that you need data from at least 6 days.")
    messagebox.showinfo("Prepare training data", "Your data has to have at least three columns. Column 'date' which contains dates of measurements; column 'Y' which contains local measurements and column 'X' which contains data from weather station.")
    data = load_csv()
    messagebox.showinfo("Data for reconstruction", "Provide data which has at least two columns: 1. 'date' which contains date, and 2. 'X' which contains weather data.")
    weather_data = load_csv()
    all_sets, train_sets, test_sets, start_date = transformation_of_data(data, n = 3)

    linear_regression_model = reconstruction_mid_term(train_sets)
    error = performance_mid_term(test_sets, linear_regression_model)
    evaluation = evaluation_mid_term_2(linear_regression_model, weather_data)

    messagebox.showinfo("Error of reconstruction", "Error of reconstruction is " + str(error))

    output_path = filedialog.asksaveasfilename(defaultextension=".csv", filetypes=[("CSV files", "*.csv")])
    if output_path:
        with open(output_path, 'w', encoding='utf-8') as f:
            evaluation.to_csv(output_path)
        messagebox.showinfo("Success!", f"Reconstruction was stored in {output_path}")
        root.destroy()
    else:
        messagebox.showwarning("Warning!", "File was not saved")
        root.destroy()

if __name__ == "__main__":
    main()
    sys.exit()