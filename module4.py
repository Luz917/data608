# -*- coding: utf-8 -*-
"""
Created on Thu Oct 15 21:11:53 2020

@author: MARYLUZ CRUZ
"""

import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import pandas as pd
import plotly.graph_objs as go

#Build dataframe

#based on module 4 sample code
soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,count(tree_id)' +\
        '&$group=spc_common,boroname,health,steward').replace(' ', '%20')


part_1 = pd.read_json(soql_url)

#part_1.shape() based on Socrata row limit, we can only get 1000 rows at a time. 
# rows get seperated then combined
part_2 = pd.read_json(soql_url + '&$offset=1000')
part_3 = pd.read_json(soql_url + '&$offset=2000')
part_4 = pd.read_json(soql_url + '&$offset=3000')
part_5 = pd.read_json(soql_url + '&$offset=4000')

nycboro = pd.concat([part_1,part_2,part_3,part_4,part_5])

nycboro = nycboro.dropna() 


###Begin Dash
app = dash.Dash()

app.layout = html.Div(children=[
    html.H2("Health of Tress in the Boroughs of New Yrok City"),

            html.Label("Select Bourough"),
            dcc.Dropdown(
                id="inboro",
                options=[{ 'label': k,'value': k} for k in ['Brooklyn', 'Bronx', 'Manhattan','Queens', 'Staten Island']],
                value='Brooklyn'
                ),
           
           html.Div(id="outputboro"),
           html.Label("Select number of Stewards"),
           dcc.Dropdown(
               id="stewin",
               options = [{'label' : k, 'value' :k } for k in nycboro['steward'].unique()],
               value = "None"
               ),
           html.Div(id="stewout")
])


@app.callback(
    dash.dependencies.Output("outputboro", "children"),
    [dash.dependencies.Input("inboro", "value")]
    )

def boros_data(input_data):
    df=nycboro[nycboro.boroname == input_data]
    return dcc.Graph(
        id="Health of Tree by Borough",
        figure= {"data":[{"x":df['health'], 'type': 'histogram','name': "Health of Tree by Borough"}],
                'layout': {'title': "Health of Tree by Borough"} 
                 }
        )

@app.callback(
    dash.dependencies.Output("stewout", "children"),
    [dash.dependencies.Input("stewin", "value")]
    )

def stew_data(input_data):
    df=nycboro[nycboro.steward == input_data]
    return dcc.Graph(
        id="Health of Steward",
        figure= {"data":[{"x":df['health'], 'type': 'histogram','name': "Health of Steward"}],
                'layout': {'title': "Health of Steward"} 
                 }
        )
    
if __name__ == '__main__':
    app.run_server(debug=True)