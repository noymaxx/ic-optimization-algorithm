import Array "mo:base/Array";
import Float "mo:base/Float";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";
import Time "mo:base/Time";
import Int "mo:base/Int";
import Iter "mo:base/Iter";

actor AntColony {

  // Define o tipo Formiga
  type Ant = {
      var position : Nat;
      var tour : [Nat];
      var tourLength : Float;
  };

  // Define o espaço do problema
  type ProblemSpace = {
      distances : [[Float]];
      numCities : Nat;
  };

  // Define a matriz de feromônios como mutável
  type PheromoneMatrix = [[var Float]];

  // Parâmetros para o algoritmo
  let alpha : Float = 1.0; // Importância dos feromônios
  let beta : Float = 2.0; // Importância da distância
  let evaporationRate : Float = 0.5;
  let Q : Float = 100.0; // Fator de depósito de feromônios

  // Inicializa o espaço do problema 
  let problemSpace : ProblemSpace = {
      distances = [
        [0.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0],
        [2.0, 0.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0],
        [3.0, 5.0, 0.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0],
        [4.0, 6.0, 7.0, 0.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0],
        [5.0, 7.0, 8.0, 9.0, 0.0, 10.0, 11.0, 12.0, 13.0, 14.0],
        [6.0, 8.0, 9.0, 10.0, 11.0, 0.0, 12.0, 13.0, 14.0, 15.0],
        [7.0, 9.0, 10.0, 11.0, 12.0, 13.0, 0.0, 14.0, 15.0, 16.0],
        [8.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 0.0, 16.0, 17.0],
        [9.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 0.0, 18.0],
        [10.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 0.0]
    ];
    numCities = 10;
  };  

  func contains<T>(arr : [T], val : T, equal : (T, T) -> Bool) : Bool {
      for (v in arr.vals()) {
        if (equal(v, val)) return true;
      };
      false
  };

    // Função auxiliar para criar um array 2D mutável preenchido com um valor padrão
    func create2DMutableArray<T>(rows : Nat, cols : Nat, defaultValue : T) : [[var T]] {
      Array.tabulate<[var T]>(rows, func(i : Nat) : [var T] {
        Array.tabulateVar<T>(cols, func(j : Nat) : T { defaultValue })
      })
    };

    // Inicializa a matriz de feromônios como mutável
    let initialPheromone : Float = 1.0;
    var pheromones : PheromoneMatrix = create2DMutableArray<Float>(problemSpace.numCities, problemSpace.numCities, initialPheromone);

    // Função para gerar um número aleatório entre 0 e max-1
    func randomNumber(max : Nat) : Nat {
      let seed = Int.abs(Time.now());
      (seed % max) : Nat
    };

    // Função auxiliar para criar um intervalo de números
    func range(start : Nat, end : Nat) : [Nat] {
      Array.tabulate<Nat>(end - start + 1, func (i : Nat) : Nat { i + start })
    };

    // Função para inicializar uma formiga em uma posição inicial aleatória
    func initializeAnt(numCities : Nat) : Ant {
      let startCity = randomNumber(numCities);
      {
          var position = startCity;
          var tour = [startCity];
          var tourLength = 0.0;
      }
  };

  // Função para inicializar uma colônia de formigas
  func initializeColony(colonySize : Nat, numCities : Nat) : [Ant] {
    Array.tabulate<Ant>(colonySize, func(_ : Nat) : Ant {  
      initializeAnt(numCities)
    })
  };

    // Função para calcular a probabilidade de mover-se para uma cidade
  func calculateProbability(ant : Ant, city : Nat, unvisitedCities : [Nat]) : Float {
    let pheromone = pheromones[ant.position][city];
    let distance = problemSpace.distances[ant.position][city];
    let numerator = Float.pow(pheromone, alpha) * Float.pow(1.0 / distance, beta);
      
    var denominator : Float = 0.0;
    for (unvisitedCity in unvisitedCities.vals()) {
      let unvisitedPheromone = pheromones[ant.position][unvisitedCity];
      let unvisitedDistance = problemSpace.distances[ant.position][unvisitedCity];
        denominator += Float.pow(unvisitedPheromone, alpha) * Float.pow(1.0 / unvisitedDistance, beta);
    };  
    numerator / denominator
  };

  // Função para selecionar a próxima cidade para uma formiga
  func selectNextCity(ant : Ant, unvisitedCities : [Nat]) : Nat {
    if (unvisitedCities.size() == 1) {
      return unvisitedCities[0];
    };

    let probabilities = Array.map<Nat, Float>(unvisitedCities, func (city : Nat) : Float {
      calculateProbability(ant, city, unvisitedCities)
    });

    var cumulativeProbability : Float = 0.0;
    let randomValue = Float.fromInt(randomNumber(1000)) / 1000.0;

    for (i in unvisitedCities.keys()) {
      cumulativeProbability += probabilities[i];
      if (cumulativeProbability >= randomValue) {
        return unvisitedCities[i];
      };
    };

    unvisitedCities[unvisitedCities.size() - 1] // Fallback para última cidade se nenhuma for selecionada
  };

  // Função para mover uma formiga para a próxima cidade
  func moveAnt(ant : Ant) {
    let unvisitedCities = Array.filter<Nat>(range(0, problemSpace.numCities - 1), func (city : Nat) : Bool {
      not contains<Nat>(ant.tour, city, Nat.equal)
    });

    if (unvisitedCities.size() > 0) {
      let nextCity = selectNextCity(ant, unvisitedCities);
      ant.tourLength += problemSpace.distances[ant.position][nextCity];
      ant.position := nextCity;
      ant.tour := Array.append<Nat>(ant.tour, [nextCity]);
    } else {
      // Retornar à cidade inicial para completar o tour
      let startCity = ant.tour[0];
      ant.tourLength += problemSpace.distances[ant.position][startCity];
      ant.position := startCity;
      ant.tour := Array.append<Nat>(ant.tour, [startCity]);
    };
  };

  // Função para atualizar os feromônios
  func updatePheromones(colony : [Ant]) {
    // Evaporação
    for (i in Iter.range(0, problemSpace.numCities - 1)) {
      for (j in Iter.range(0, problemSpace.numCities - 1)) {
        pheromones[i][j] *= (1.0 - evaporationRate);
      };
    };

    // Depósito
    for (ant in colony.vals()) {
      let pheromoneDeposit = Q / ant.tourLength;
      for (i in Iter.range(0, ant.tour.size() - 2)) {
        let fromCity = ant.tour[i];
        let toCity = ant.tour[i + 1];
        pheromones[fromCity][toCity] += pheromoneDeposit;
        pheromones[toCity][fromCity] += pheromoneDeposit; // Gráfico simétrico
      };
    };
  };

  // Função principal de otimização por colônia de formigas 
  public func antColonyOptimization(maxIterations : Nat, colonySize : Nat) : async [Nat] {
    var bestTour : [Nat] = [];
    var bestTourLength : Float = 1e38; // Reset para cada execução

    for (_ in Iter.range(0, maxIterations - 1)) {
      var colony = initializeColony(colonySize, problemSpace.numCities);

      // Move as formigas
      for (_ in Iter.range(0, problemSpace.numCities - 1)) {
        for (ant in colony.vals()) {
          moveAnt(ant);
        };
      };

      // Atualiza o melhor tour 
      for (ant in colony.vals()) {
        if (ant.tourLength < bestTourLength) {
          bestTour := ant.tour;
          bestTourLength := ant.tourLength;
        };
      };
  
      // Atualiza feromônios
      updatePheromones(colony);
    };

    bestTour
  };
};
