
from podr_connection import podr_connection
from cdisc_library import CDISCConnector
from load_specifications import load_usecase_specification
from dotenv import load_dotenv
from yaml import load
import re

import os
# load the environment variables
load_dotenv(dotenv_path=os.path.join(os.path.dirname(__file__), '..', ".env"))


def extract_dataset(content):
    patt = re.compile("([A-Z]{2,4})\.([A-Z]+)")
    return patt.findall(content)


class Field:
    def __init__(self, dataset, variable, codelist, source_derivation, origin):
        self._dataset = dataset
        self._variable = variable
        self._codelist = codelist if codelist else None
        self._raw_source = source_derivation
        self._origin = origin
        self._source_variable = None
        self._dependencies = None
        self._cdisc = None
        
    @property
    def source_variable(self):
        if self._origin == "Copied from Source":
            return self._raw_source
        return
    
    @property
    def dependencies(self):
        pass
    
    def merge_config(self, config):
        if config.get('dependencies'):
            pass


class Dataset:
    
    def __init__(self, name, standard, version=None):
        self._name = name
        self._standard = standard
        self._version  = version
        self._specification_connection = None
        self._data_connection = None
        self._variables = {}
        self._controlled_terminology = {}
        self._db_cache = {}
        self._specs = []
        self._ct = None
        
    @property
    def library(self):
        if self._specification_connection is None:
            self._specification_connection = CDISCConnector(os.environ["CDISC_USER"], os.environ["CDISC_PASS"])
        return self._specification_connection
            
    def fetch_dataset(self, domain):
        """
        Get the data
        """
        if self._data_connection is None:
            self._data_connection = podr_connection(os.environ["PODR_USER"], os.environ["PODR_PASS"])
        self._db_cache[domain] = self._data_connection.read(domain, "janssen_synthetic")
        
    def fetch_specification(self):
        """
        Fetch the upstream data specifications
        """
        if self._standard in self.library.products:
            dataset = self.library
        else:
            print(f"No upstream specifications for standard {self._standard}")
    
    def _warm(self, domain):
        if not domain in self._db_cache and domain != self._name:
            self.fetch_dataset(domain)
            
    def load_specifications(self):
        """
        Load the specifications
        """
        specs, ct = load_usecase_specification(self._name)
        if specs is None:
            print(f"Unable to load specifications for {self._name}")
        self._ct = ct
        for row in specs:
            if row["Derived or Copied from Source Variable"] == "Copied from Source":
                source = row["Source/Derivation"]
                if source.split("."):
                    # pre fetch the data
                    domain, variable = source.split(".")
                    self._warm(domain)
            else:
                fields = extract_dataset(row["Source/Derivation"])
                for (domain, variable) in fields:
                    self._warm(domain) 
                    
    def load_derivations(self):
        if os.path.exists(f"{self._name}.yaml"):
            with open(f"{self._name}.yaml", 'r') as fh:
                derived = yaml.load(fh)
        
    
    def render(self):
        """
        Use the specifications to generate a dataset 
        """
        pass
    
        