import numpy as np
import random
import sys

class ParticleSwarmOptimization:
    def __init__(self, func, funcFeasible, A_ub, b_ub, A_eq, b_eq, bounds, N=100, MaxIter=50, c1=0.9, c2=0.9, w=0.9):
        self.func = func
        self.funcFeasible = funcFeasible
        self.A_ub = np.array(A_ub)
        self.b_ub = np.array(b_ub)
        self.A_eq = np.array(A_eq)
        self.b_eq = np.array(b_eq)
        self.bounds = np.array(bounds)
        self.N = N
        self.MaxIter = MaxIter
        self.c1 = float(c1)
        self.c2 = float(c2)
        self.w = float(w)
        self.nvar = 0
        self.particles = None
        self.fitness = None
        self.P_best = None
        self.Pf_best = None
        self.G_best = None
        self.Gf_best = np.inf
        self.velocity = None
    
    
    def create_particles(self):
        self.particles = np.zeros((self.N, self.nvar), dtype=float)
        self.velocity = np.zeros((self.N, self.nvar), dtype=float)
        self.fitness = np.zeros((self.N), dtype=float)
        
        # Cambiar los límites si se pide ninguno.
        for v in range(self.nvar):           
            if self.bounds[v, 0] == None:
                self.bounds[v, 0] = float(-1000000)
            if self.bounds[v, 1] == None:
                self.bounds[v, 1] = float(1000000)
        
        # Partículas y velocidad.
        for i in range(self.N):
            for v in range(self.nvar): 
                vmin, vmax = self.bounds[v, 0], self.bounds[v, 1]
                self.particles[i, v] = np.random.uniform(vmin, vmax)
                self.velocity[i, v] = np.random.uniform(-(vmax-vmin), vmax-vmin)
            
            while self.funcFeasible(self.particles[i], self.A_ub, self.b_ub, self.A_eq, self.b_eq, self.bounds) == False:
                for v in range(self.nvar): 
                    vmin, vmax = self.bounds[v, 0], self.bounds[v, 1]
                    self.particles[i, v] = np.random.uniform(vmin, vmax)
                    
    def particle_fitness(self):
        i = 0
        for P in self.particles:       
            self.fitness[i] = self.func(P)
            i += 1     
            
    def update_velocity(self, index):
        # Crear nuevo array de velocidad.
        new_velocity = np.zeros((self.nvar), dtype=float)
        
        # Calcular la nueva velocidad.
        for t in range(self.nvar):
            # Variables aleatorias.
            r1, r2 = np.random.uniform(0,1,2)
        
            inertia = self.w * self.velocity[index,t]
            individual_component = self.c1*r1*(self.P_best[index,t]-self.particles[index,t])
            social_component = self.c2*r2*(self.G_best[t]-self.particles[index,t])
            
            new_velocity[t] = inertia + individual_component + social_component
            
        return new_velocity
    
    def relocate(self, index, P):
        return np.sum([self.particles[index],P],axis=0) / 2
         
    def update_position(self, index):
        # Sumar velocidad.
        new_position = self.particles[index] + self.velocity[index]
        
        # Verificar que la partícula cumpla las restricciones.
        while True:
            isValid = self.funcFeasible(new_position, self.A_ub, self.b_ub, self.A_eq, self.b_eq, self.bounds)
            
            if isValid == True:
                break
                
            new_position = self.relocate(index, new_position)
        
        # Valor de la función objetivo de la nueva posición.
        new_position_fitness = self.func(new_position)
        
        return new_position, new_position_fitness
                
            
            
        
    def solve(self):
        # Obtener número de variables.
        self.nvar = len(self.bounds)
        
        # Crear partículas.
        self.create_particles()
        
        # Calcular valores de la función objetivo de las partículas.
        self.particle_fitness()
        
        # Inicializar P_best y sus valores de la función objetivo.
        self.P_best = np.copy( self.particles ) # Las mejores partículas.
        self.Pf_best = np.copy( self.fitness ) # Valores de la función objetivo de las mejores partículas. 
        
        # Inicializar G_best y sus valores de la función objetivo.
        index = np.argmin(self.fitness) # Obtener el índice del mejor.
        self.G_best = np.copy( self.particles[index] ) # La mejor partícula.
        self.Gf_best = self.fitness[index] # Valor de la función objetivo de la mejor partícula.
        
        # Fraccionar c1, c2, w.
        factor = 0.1
        c1_frac = (self.c1-factor) /  self.MaxIter
        c2_frac = (self.c2-factor) /  self.MaxIter
        w_frac = (self.w-factor) /  self.MaxIter
        
        it = 0
        while it < self.MaxIter:
            i = 0
            for P in self.particles:
                # Actualizar velocidad.
                self.velocity[i] = self.update_velocity(i)
                
                # Actualizar posición.
                self.particles[i], self.fitness[i] = self.update_position(i)
                
                # Actualizar P_best.
                if self.fitness[i] < self.Pf_best[i]:
                    self.P_best[i] = np.copy( self.particles[i] )
                    self.Pf_best[i] = np.copy( self.fitness[i] )
                    
                # Actualizar G_best.
                if self.fitness[i] < self.Gf_best:
                    self.G_best = np.copy( self.particles[i] )
                    self.Gf_best = np.copy( self.fitness[i] )
                    
                i += 1   
                
            # Crementar c1, c2, w.
            self.c1 -= c1_frac
            self.c2 -= c2_frac
            self.w -= w_frac
            
            # Aumentar iterador.
            it += 1
            
        res = {
            "fun": self.Gf_best,
            "nit": self.MaxIter,
            "x": self.G_best
        }
        return res
    

def particle_swarm(func, funcFeasible, A_ub, b_ub, A_eq, b_eq, bounds, N=100, MaxIter=50, c1=0.9, c2=0.9, w=0.9):
    pso = ParticleSwarmOptimization(func, funcFeasible, A_ub, b_ub, A_eq, b_eq, bounds, N, MaxIter, c1, c2, w)
    return pso.solve()