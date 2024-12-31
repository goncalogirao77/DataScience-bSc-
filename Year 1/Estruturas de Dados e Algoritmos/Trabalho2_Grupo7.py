import csv
import networkx as nx
import matplotlib.pyplot as plt
import folium
import random
import math


class LondonNetworkGraph:
    def __init__(self):
        self.graph = nx.DiGraph()

    @staticmethod
    def list_stations():
        stations_list = []
        with open('stations.csv') as stations1:
            stations2 = csv.reader(stations1, delimiter=',')
            next(stations2)
            next(stations2)
            for station in stations2:
                stations_list.append(station)
        return stations_list

    def stations(self):
        stations_list = self.list_stations()
        for station in stations_list:
            pos = float(station[1]), float(station[2])
            self.graph.add_node(int(station[0]), pos=pos, zone=float(station[5]), n_lines=float(station[6]),
                                name=station[3])

    @staticmethod
    def list_connections():
        list_connections = []
        with open("connections.csv") as connections1:
            connections2 = csv.reader(connections1, delimiter=',')
            next(connections2)
            for line in connections2:
                list_connections.append(line)
        return list_connections

    def connections(self):
        connections = self.list_connections()
        for line in connections:
            self.graph.add_edge(int(line[1]), int(line[2]), distance=float(line[3]), line=int(line[0]),
                                tnormal=float(line[4]), t7_10=float(line[5]), t10_16=float(line[6]))

    def n_stations(self):
        return self.graph.number_of_nodes()

    def n_stations_zones(self):
        zone_count = {}
        for node, arrt in self.graph.nodes(data=True):
            zona = arrt['zone']

            if zona - int(zona) != 0:
                menor = int(zona)
                maior = int(zona + 0.5)

                if menor in zone_count:
                    zone_count[menor] += 1
                else:
                    zone_count[menor] = 1
                if maior in zone_count:
                    zone_count[maior] += 1
                else:
                    zone_count[maior] = 1
            else:
                if zona in zone_count:
                    zone_count[zona] += 1
                else:
                    zone_count[zona] = 1

        for z, n in zone_count.items():
            print(f'A zona {z} tem {n} estações')
        return

    def n_edges(self):
        return self.graph.number_of_edges()

    def n_edges_line(self):
        edges = {}
        for start, end, arrt in self.graph.edges(data=True):
            if arrt['line'] not in edges.keys():
                edges[arrt['line']] = 1
            else:
                edges[arrt['line']] += 1
        for e, n in edges.items():
            print(f'A linha {e}, tem {n} edges')

    def mean_degree(self):
        soma = 0
        total = 0
        for node, arrt in self.graph.nodes(data=True):
            soma += (arrt['n_lines'] * 2)
            total += 1
        return soma/total

    def mean_weigth(self, weight):
        total = 0
        n = self.n_edges()
        for start, end, arrt in self.graph.edges(data=True):
            total += int(arrt[weight])
        return total/n

    def visualize_sqtr(self):
        x = []
        y = []
        for start, end, arrt in self.graph.edges(data=True):
            coords1 = self.graph.nodes[start]['pos']
            coords2 = self.graph.nodes[end]['pos']
            plt.plot([coords1[1], coords2[1]], [coords1[0], coords2[0]], color='darkblue', marker='o', markersize=3.5)
        plt.scatter(x, y)
        plt.show()
        return

    def visualize_nx(self):
        nx.draw(self.graph, with_labels=True)
        plt.show()
        return

    def visualize_graph(self):
        mapa = folium.Map(location=[51.5074, -0.1278], zoom_start=12)

        for node, arrt in self.graph.nodes(data=True):
            folium.Marker(arrt['pos'], popup=arrt['name']).add_to(mapa)

        for start, end, arrt in self.graph.edges(data=True):
            coords1 = self.graph.nodes[start]['pos']
            coords2 = self.graph.nodes[end]['pos']
            folium.PolyLine([coords1, coords2], color='darkblue').add_to(mapa)

        mapa.save("LondonMetroNetwork.html")
        return

    @staticmethod
    def gera_ponto():
        x = random.uniform(51.3962, 51.7090)
        y = random.uniform(-0.6110, 0.1159)
        return x, y

    @staticmethod
    def gera_hora():
        return random.randint(0, 23)

    @staticmethod
    def distancia(ponto1, ponto2):
        return math.sqrt((float(ponto1[0])-float(ponto2[0]))**2 + (float(ponto1[1])-float(ponto2[1]))**2)

    def station_prox(self, ponto):
        min_distancia = float('inf')
        prox = None
        for start, end, arrt in self.graph.edges(data=True):
            ponto2 = self.graph.nodes[start]['pos']
            distancia = self.distancia(ponto, ponto2)
            if distancia <= min_distancia:
                min_distancia = distancia
                prox = start
        return prox

    def dijkstra_nx(self):
        s = self.gera_ponto()
        start = self.station_prox(s)
        print(f'Start: {start}')
        e = self.gera_ponto()
        end = self.station_prox(e)
        print(f'End: {end}')
        hora = self.gera_hora()
        print(f'Hour: {hora}')
        caminho = nx.dijkstra_path(self.graph, start, end, weight='tnormal')
        if 7 <= hora < 10:
            caminho = nx.dijkstra_path(self.graph, start, end, weight='t7_10')
        elif 10 <= hora < 16:
            caminho = nx.dijkstra_path(self.graph, start, end, weight='t10_16')
        return s, e, caminho

    def dijkstra(self):
        s = self.gera_ponto()
        start = self.station_prox(s)
        e = self.gera_ponto()
        end = self.station_prox(e)
        hora = self.gera_hora()

        caminho = {start: [start]}

        graph = {}
        for c in self.list_connections():
            source = int(c[1])
            target = int(c[2])
            weight = float(c[4])
            t7_10 = float(c[5])
            t10_16 = float(c[6])
            if 7 < hora <= 10:
                weight = t7_10
            if 10 < hora <= 16:
                weight = t10_16
            if source not in graph:
                graph[source] = []
            graph[source].append((target, weight))

        distances = {node: float('inf') for node in graph}
        distances[start] = 0

        visited = set()
        while len(visited) < len(graph):
            min_distance = float('inf')
            min_node = None

            for node in graph:
                if node not in visited and distances[node] < min_distance:
                    min_distance = distances[node]
                    min_node = node

            visited.add(min_node)

            if min_node in graph:
                for neighbor, weight in graph[min_node]:
                    if neighbor not in visited:
                        distance = distances[min_node] + weight
                        if distance < distances[neighbor]:
                            distances[neighbor] = distance
                            caminho[neighbor] = caminho[min_node] + [neighbor]

        return s, e, caminho[end]

    def mostrar_caminho(self, data):
        s, e, caminho = data
        mapa = folium.Map(location=[51.5074, -0.1278], zoom_start=12)
        folium.Marker(s).add_to(mapa)
        folium.Marker(e).add_to(mapa)

        for start, end, arrt in self.graph.edges(data=True):
            coords1 = self.graph.nodes[start]['pos']
            coords2 = self.graph.nodes[end]['pos']
            folium.PolyLine([coords1, coords2], color='darkgreen').add_to(mapa)

        for n in range(0, len(caminho)-1):
            coords1 = self.graph.nodes[caminho[n]]['pos']
            coords2 = self.graph.nodes[caminho[n+1]]['pos']
            folium.PolyLine([coords1, coords2], color='red').add_to(mapa)
        mapa.save("DijkstraNX.html")
        return


if __name__ == "__main__":
    L = LondonNetworkGraph()
    L.stations()
    L.connections()
