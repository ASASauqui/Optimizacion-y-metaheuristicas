import numpy as np
import random

class DifferentialEvolution:
    def __init__(self, func, bounds, args=(), popsize=30, ngen=100, cr=0.5):
        self.func = func
        self.bounds = np.array(bounds)
        self.args = args
        self.popsize = popsize
        self.ngen = ngen
        self.cr = cr
        self.population = None
        self.fitness = None
        self.pop_range = None
        self.nfev = 0
        self.nvar = 0
        
    def init_population(self):
        self.population = np.zeros((self.popsize, self.nvar), dtype=float)
        self.fitness = np.zeros((self.popsize), dtype=float)
        
        for v in range(self.nvar):
            vmin, vmax = self.bounds[v, 0], self.bounds[v, 1]
            self.population[:, v] = np.random.uniform(vmin, vmax, self.popsize)
        
        i = 0
        for P in self.population:       
            self.fitness[i] = self.func(P, *self.args)
            self.nfev += 1
            i += 1
            
        
    def mutation(self):
        random_selection = random.sample(self.pop_range, 3)
        xr1 = self.population[random_selection[0]]        
        xr2 = self.population[random_selection[1]]
        xr3 = self.population[random_selection[2]]
        v = np.zeros_like(xr1)
        f = np.random.uniform(1e-1000, 2)
        v = xr1 + f*(xr2 - xr3)
        for i in range(self.nvar):
            if v[i] < self.bounds[i, 0]:
                v[i] = self.bounds[i, 0]
            if v[i] > self.bounds[i, 1]:
                v[i] = self.bounds[i, 1]
        return v
    
    def crossover(self, x, v):
        u = np.zeros_like(x)
        
        for var in range(self.nvar):
            r = np.random.uniform(0, 1)
            if r < self.cr:               
                u[var] = v[var]                
            else:
                u[var] = x[var]
        return u                                     
    
    def population_fitness(self):
        i = 0
        for x in self.population:            
            self.fitness[i] = self.func(x, *self.args)            
            self.nfev += 1
            i += 1
                
    def selection(self, i, u, u_fitness):
        if u_fitness < self.fitness[i]:
            self.population[i], self.fitness[i] = u, u_fitness
        
    def solve(self):
        self.nvar = len(self.bounds)
        self.pop_range = range(0,self.popsize)
        self.init_population()
        g = 0
        while g < self.ngen:
            i = 0
            for x in self.population:
                v = self.mutation()                
                u = self.crossover(x, v)
                u_fitness = self.func(u, *self.args)
                self.nfev += 1
                self.selection(i, np.copy(u), u_fitness)
                i += 1        
            g += 1                
        res = {
            "P": self.population[np.argmin(self.fitness)],
            "nIt": self.ngen,
            "fun": np.min(self.fitness),
            "nFev": self.nfev
        }
        return res
    

def differential_algorithm(func, bounds, args=(), popsize=30, ngen=100, cr=0.5):    
    de = DifferentialEvolution(func, bounds, args, popsize, ngen, cr)
    return de.solve()