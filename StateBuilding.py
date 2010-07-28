#!/usr/bin/env python
# encoding: utf-8
"""
StateBuilding.py

Purpose:    Code in support of "Network, collective action, and state building"
            
            Code contains two classes: Agent and Environment.
            
            Agent class:        Agent object for computational model described in above paper.
                                Contains fuctionality for forming agent networks,
                            
            Environment class:  Class object to contain agents, and object in which the 
                                computational model is run.  Contains functionality
                                for creating agent networks, adjudicating controbution level,
                                and outputting data from runs.

Author:     Drew Conway
Email:      drew.conway@nyu.edu
Date:       2010-07-13

Copyright (c) 2010, under the Simplified BSD License.  
For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
All rights reserved.
"""

import sys
import os
import unittest
import networkx as nx
from numpy.random import randint,uniform,pareto,random
import csv


class Agent(object):
    """Agent objet
    
    Parameters
    
        agent_id:       Unique integer identifier for each agent
    
    """
    def __init__(self, agent_id,agent_type=None):
        # First two agent parameters are exogenous primitives 
        self.disposition=randint(low=0,high=2)  # Prior disposition to contribuiting 
        self.wealth=pareto(3.)      # Level of wealth
        # Next, endogenously generated variables
        # Set agent type
        if type(agent_id) is not int:
            raise ValueError("Agent IDs must be integers")
        else:
            self.my_id=agent_id     # Agent identifier
        self.adj_list=list()        # Agent's ego-network (adjacency network)
        # Set agent type, from one of five possible type:
        #   0 - Altruistic: Always sets c=.5(wealth)
        #   1 - Community:  Sets c=m' such that m'+m_net=w, given m_net
        #   2 - Min-match:  Sets c=min(m') for all of agent's neighbors
        #   3 - Max-match:  Sets c=max(m') for all of agent's neighbors
        #   4 - Miserly:    Sets c=\epsilon
        if agent_type is None:
            self.type=randint(low=0,high=5)
        else:
            if type(agent_type)is int and agent_type>=0 and agent_type<5:
                self.type=agent_type
            else:
                raise ValueError("Agent type must be an int between 0 and 4")
        # Finally, place holder for level of contribution and m_net
        self.contrib=None
        self.mnet=None
    
    def make_tie(self, tie_agent):
        """Add an agent to adjacency list"""
        self.adj_list.append(tie_agent.get_id())
        
    def get_id(self):
        """Returns agent identification"""
        return self.my_id
        
    def get_neighbors(self):
        """Returns a list of agent's neighbors"""
        return self.adj_list
        
    def get_egonet(self, as_graph=False):
        """Returns agents ego network, either as
        an NX edgelist list, or a NX Graph object
        """
        if as_graph:
            return nx.Graph(data=zip([(self.my_id) for i in self.adj_list],self.adj_list))
        else:
            return zip([self.get_id() for n in self.get_neighbors()],self.get_neighbors())
        
    def get_disposition(self):
        """Returns an agent's disposition to support"""
        return self.disposition
        
    def get_wealth(self):
        """Returns an agent's exogenous wealth"""
        return self.wealth
        
    def set_contrib(self,contrib_level):
        """Sets the agent's c parameter"""
        if contrib_level>=0 and contrib_level <=1:
            self.contrib=contrib_level
        else:
            raise ValueError("Contribution level must be \in[0,1]")
            
    def get_contrib(self):
        """Returns agent's c parameter"""
        return self.contrib
        
    def get_type(self):
        """Returns agent's type parameter"""
        return self.type
        
    def set_mnet(self,mnet):
        """Sets the agents m_net parameter"""
        self.mnet=mnet
        
    def get_mnet(self):
        """Returns agent's mnet parameter"""
        return self.mnet
        
    def info(self):
        """Print agent parameter values to STDOUT"""
        print("Agent:   "+str(self.get_id()))
        print("Wealth:  "+str(self.get_wealth()))
        print("Dispo:   "+str(self.get_disposition()))
        print("Type:    "+str(self.get_type()))
        print("Neigh:   "+str(self.get_neighbors()))
        print("Contrib: "+str(self.get_contrib()))
        
        
class Environment(object):
    """The Environment in which the game is played
    
    Parameters
    
        population:     Number of agents in the population
        degree_seq:     Optional degree sequence to configure agent network (default
                        network a preferential attachment model based on wealth)
        m:              Optional fraction of total state wealth required to provide public good, i.e. the threhold.
                        As such, the value of m must \in[0,1], by default m=.25*state_wealth (1/4 of a state's total wealth)
                        
    NOTE: BY INITIALIZAING THIS OBJECT YOU ARE---IN EFFECT---RUNNING A SIMULATION
    
    """
    def __init__(self, population,degree_seq=None,m=None):
        # Create a population of agents
        self.agents=list()
        if type(population)==int and population>0:
            for a in range(population):
                self.agents.append(Agent(agent_id=a))
        else:
            raise ValueError("Model must have positive number of agents")
        # Get total wealth in state
        self.state_wealth=sum(map(lambda a: a.get_wealth(),self.agents))
        if m is None:
            self.threshold=.25*self.state_wealth
        else:
            if m>=0 and m<=1:
                self.threshold=m*self.state_wealth
            else:
                raise ValueError("Value for m must be between 0 and 1")
        # Create network
        if degree_seq is None:
        # If no degree sequence is provided create wealth-based preferential attachment
        # This is the default setting for the model
            for i in xrange(population):
                for j in xrange(population):
                    if i!=j:
                        prob_tie=uniform(low=0,high=1)
                        # Tie probability function of agent's wealth relative to 
                        # total wealth in state
                        if prob_tie<=self.agents[j].get_wealth()/self.state_wealth:
                        # Create symmetric ties between neighbors
                            self.agents[i].make_tie(self.agents[j])
                            self.agents[j].make_tie(self.agents[i])
        else:
            if(nx.is_valid_degree_sequence(degree_seq) and len(degree_seq)==population):
                # Use NX configuration model to create network from degree sequence. By default,
                # the configuration model returns a MultiGraph type with edges assigned at random.
                # For consistency, the network type returned is Graph, and the random seed is
                # always set to the number of agents in the environment.
                G=nx.generators.configuration_model(degree_seq,create_using=nx.Graph(),seed=population)
                for e in G.edges():
                    self.agents[e[0]].make_tie(self.agents[e[1]])
                    self.agents[e[1]].make_tie(self.agents[e[0]])
            else:
                raise nx.NetworkXError('Invalid degree sequence')
        # Calculate all agent's m_net parameter
        for a in self.agents:  
            agent_neighbors=a.get_neighbors()
            # Get wealth of all neighbors
            y_net=sum(map(lambda n: self.agents[n].get_wealth(),agent_neighbors))
            # Calculate m_net
            m_net=0
            for n in agent_neighbors:
                n_wealth=self.agents[n].get_wealth()
                n_disposition=self.agents[n].get_disposition()
                m_net+=n_disposition*(n_wealth/y_net)
            a.set_mnet(m_net)
        # Set all agents contribution levels based on their network position
        for a in self.agents:
            # Get all relevant agent info
            agent_type=a.get_type()
            agent_mnet=a.get_mnet()
            agent_wealth=a.get_wealth()
            agent_neighbors=a.get_neighbors()
            if agent_type == 0:
            # Altruistic type
                a.set_contrib(0.5*a.get_disposition())
            else:
                if agent_type==1:
                # Community type
                    unmet=self.threshold-agent_mnet # Level of weath needed to meet threshold
                    if unmet>0:
                        if unmet<agent_wealth:
                            a.set_contrib((unmet/agent_wealth)*a.get_disposition())
                        else:
                        # Agent commits all wealth if threshold out of reach
                            a.set_contrib(1.0*a.get_disposition())
                    else:
                        a.set_contrib(0.0)
                else:
                    if agent_type==2:
                    # Min-match type
                        if agent_mnet>0:
                            min_neighbor=min(map(lambda n: self.agents[n].get_wealth(),agent_neighbors))
                            min_prop=min_neighbor/agent_mnet
                            if min_prop<1:
                                a.set_contrib(min_prop*a.get_disposition())
                            else:
                                a.set_contrib(1.0*a.get_disposition())
                        else:
                            a.set_contrib(random()*a.get_disposition())
                    else:
                        if agent_type==3:
                        # Max-match type
                            if agent_mnet>0:
                                max_neighbor=max(map(lambda n: self.agents[n].get_wealth(),agent_neighbors))
                                max_prop=max_neighbor/agent_mnet
                                if max_prop<1:
                                    a.set_contrib(max_prop*a.get_disposition())
                                else:
                                    a.set_contrib(1.0*a.get_disposition())
                            else:
                                a.set_contrib(random()*a.get_disposition())
                        else:
                            a.set_contrib(uniform(0.0,0.05)*a.get_disposition())
        # Finally, check to see if threshold has been met
        self.total_contribs=sum(map(lambda a: a.get_contrib()*a.get_wealth(),self.agents))
        if(self.total_contribs>=self.threshold):
            self.threshold_met=True
        else:
            self.threshold_met=False

    def get_population(self):
        """Return list of Agent classes"""
        return self.agents
        
    def num_agents(self):
        """Returns the number of agents in model"""
        return len(self.agents)
        
    def get_state_wealth(self):
        """Returns sum of wealth of all agents"""
        return self.state_wealth
        
    def get_agent(self, agent_id):
        """Return the agent with given ID"""
        try: 
            return self.agents[agent_id]
        except IndexError:
            print("No agent at index "+str(agent_id)+". None returned")
            return None
            
    def get_agent_ids(self):
        """Returns a list of agent IDS"""
        return map(lambda a: a.get_id(),self.get_population())
        
    def get_total_contribs(self):
        """Returns total number of contributions from agents"""
        return self.total_contribs
        
    def good_provided(self):
        """Returns boolean for wether necessary level of contributions
        were met"""
        return self.threshold_met
        
    def get_threshold(self):
        """Returns public goods provision theshold for given environment"""
        return self.threshold
        
    def get_contribs(self,as_dict=False):
        """By default, returns the sum of contributions made by agents.
        If as_sum=False, then returns a dict of contributions keyed by 
        agent ids."""
        if as_dict:
            contrib_dict=dict.fromkeys(self.get_agent_ids())
            for k in contrib_dict.keys():
                contrib_dict[k]=self.get_population()[k].get_contrib()*self.get_population()[k].get_wealth()
            return contrib_dict
        else:
            return self.total_contribs
        
    def get_network(self,robust=False):
        """Returns the whole social network of all agents in 
        the simulation as a NX Graph object
        """
        agent_ids=self.get_agent_ids()
        social_net=nx.Graph()
        social_net.add_nodes_from(agent_ids)
        for a in agent_ids:
            new_edges=self.get_agent(a).get_egonet(as_graph=False)
            social_net.add_edges_from(new_edges)
        social_net.name="Social Network"
        if robust:
            nx.info(social_net)
        return social_net
     
    ### DATA OUTPUT FUNCTIONS ### 
    def write_network(self,path):
        """Returns a NX edgelist file"""
        try:
            nx.write_pajek(self.get_network(),path)
        except IOError:
            print("Error file path: "+str(path))
            
    def get_data(self,csv_path=None):
        """Returns a dict of all relevant data from model"""
        model_data={"population": self.num_agents(), "state_wealth": self.get_state_wealth(), "threshold": self.threshold, "contribs": self.get_contribs(), "threshold_met": int(self.good_provided())}
        agent_data=dict.fromkeys(self.get_agent_ids())
        for a in xrange(self.num_agents()):
            current_agent=self.get_population()[a]
            agent_data[a]={}
            agent_data[a]["wealth"]=current_agent.get_wealth()
            agent_data[a]["disposition"]=current_agent.get_disposition()
            agent_data[a]["type"]=current_agent.get_type()
            agent_data[a]["num_neighbors"]=len(current_agent.get_egonet())
            agent_data[a]["contrib"]=current_agent.get_contrib()
        model_data["agent_data"]=agent_data
        if csv_path is not None:
            fn=agent_data[0].keys()
            fn.extend(["threshold","threshold_met"])
            writer=csv.DictWriter(open(csv_path, "w"), fieldnames=fn)
            writer.writerow(dict([(a,a) for a in fn]))
            for a in agent_data.keys():
                row=agent_data[a]
                row["threshold"]=model_data["threshold"]
                row["threshold_met"]=model_data["threshold_met"]
                writer.writerow(row)
        return model_data


class TestAgent(unittest.TestCase):
    """Test case for Agent class"""
    
    agent=None
    id_val=1
    
    def setUp(self):
        """Initialize an Agent class"""
        self.agent=Agent(agent_id=self.id_val)
        
    def test_get_id(self):
        """Test that Agent stores ID value correctly"""
        self.assertEquals(self.agent.get_id(),self.id_val)
        
    def test_tie_egonet(self):
        """Test to verify that ties are stored correctly, 
        and the network is returned correctly
        """
        new_id=2
        tie_agent=Agent(agent_id=new_id)
        self.agent.make_tie(tie_agent)
        self.assertEquals(self.agent.get_egonet(as_graph=False),[(self.id_val,new_id)])
        self.assertTrue(nx.is_isomorphic(self.agent.get_egonet(as_graph=True),nx.Graph(data=[(1,2)])))
        
    def test_disposition(self):
        """Test to verify that the dispostion is 
        stored in range.
        """
        d=self.agent.get_disposition()
        self.assertTrue(d==0 or d==1)
    
    def test_wealth(self):
        """Test to verify that the exogenous wealth 
        is stored in range.
        """
        w=self.agent.get_wealth()
        self.assertTrue(w>=0)
        
    def test_contrib(self):
        """Check that agent contribution level is 
        setting and getting correctly"""
        self.assertTrue(self.agent.get_contrib() is None)
        new_c=random()
        self.agent.set_contrib(new_c)
        self.assertEquals(new_c,self.agent.get_contrib())
        
    def test_type(self):
        """Tests that type is an integer \in{0,1,2,3,4}"""
        agent_type=self.agent.get_type()
        self.assertTrue(type(agent_type) is int and agent_type>=0 and agent_type<=4)
        
               
class TestEnvironment(unittest.TestCase):
    """Test case for Environment class"""
    
    environment_default=None
    environment_config=None
    pop=100
    ds=nx.create_degree_sequence(pop,nx.utils.powerlaw_sequence)
    G=nx.configuration_model(ds,create_using=nx.Graph(),seed=pop)
    
    def setUp(self):
        """Initialize an Environment object"""
        self.environment_default=Environment(population=self.pop)
        self.environment_config=Environment(population=self.pop,degree_seq=self.ds)
        
    def test_num_agents(self):
        """Test to verify that the number of agents 
        equals the given number
        """
        self.assertEquals(self.pop,len(self.environment_default.get_population()))
        self.assertEquals(self.pop,len(self.environment_config.get_population()))
        
    def test_get_agent(self):
        """Test to verify that agent retrieval works properly"""
        test_id=0
        test_agent=self.environment_default.get_agent(test_id)
        none_agent=self.environment_default.get_agent(self.pop)
        self.assertEquals(test_id,test_agent.get_id())
        self.assertEquals(None,none_agent)
        
    def test_agent_ids(self):
        """Test to verify that nodes are added correctly"""
        test_ids=self.environment_default.get_agent_ids()
        self.assertEquals(test_ids,range(self.pop))

    def test_network(self):
        """Test to verify that network has been generated"""
        # Default model first
        default_net=self.environment_default.get_network(robust=True).to_directed() # Only for test
        # Test that edges have been added correctly
        # Get all edges
        default_edges=list()
        for a in self.environment_default.get_population():
            add_edges=a.get_egonet(as_graph=False)
            for e in add_edges:
                if default_edges.count(e)==0:
                    default_edges.append(e)
        def_net_edges=default_net.edges()
        self.assertEquals(len(def_net_edges),len(default_edges))
        # Configuration model next
        config_net=self.environment_config.get_network(robust=True).to_directed()
        config_edges=list()
        for a in self.environment_config.get_population():
            add_edges=a.get_egonet(as_graph=False)
            for e in add_edges:
                if config_edges.count(e)==0:
                    config_edges.append(e)
        con_net_edges=config_net.edges()
        self.assertEquals(len(con_net_edges),len(config_edges))
        
    def test_treshold(self):
        """Tests to verify that treshold parameter set correctly"""
        #Default model first
        default_contrib=self.environment_default.get_contribs(as_dict=True)
        if sum(default_contrib.values())>=self.environment_default.get_threshold():
            self.assertTrue(self.environment_default.good_provided())
            self.assertEquals(sum(default_contrib.values()),self.environment_default.get_contribs())
        else:
            self.assertFalse(self.environment_default.good_provided())
            
    def test_data(self):
        """Tests that all data retrieval works properly"""
        test_data=self.environment_default.get_data("test.csv")
        # Series of tests specific to model data
        self.assertTrue(test_data["population"]>0)
        self.assertTrue(test_data["state_wealth"]>0)
        self.assertTrue(test_data["threshold"]>=0 and test_data["threshold"]<=self.environment_default.get_state_wealth())
        self.assertTrue(test_data["contribs"]<=self.environment_default.get_state_wealth())
        self.assertTrue(test_data["threshold_met"]==0 or test_data["threshold_met"]==1)
        test_agent=test_data["agent_data"]
        # Series of tests specific to agent data
        for i in test_agent.keys():
            self.assertTrue(test_agent[i]["wealth"]>0)
            self.assertTrue(test_agent[i]["disposition"]==0 or test_agent[i]["disposition"]==1)
            self.assertTrue(test_agent[i]["type"]>=0 and test_agent[i]["type"]<=4)
            self.assertTrue(test_agent[i]["num_neighbors"]>=0)
            self.assertTrue(test_agent[i]["contrib"]>=0)
        # Test CSV output
        

if __name__ == '__main__':
    unittest.main()
    


