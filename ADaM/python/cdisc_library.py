import requests


class CDISCConnector:
    BASE_URL = "https://library.cdisc.org/api/"
    def __init__(self, username, password):
        self._client = None
        self._username = username
        self._password = password
        self._cache = {}
        
    @property
    def client(self):
        if self._client is None:
            session = requests.Session()
            session.auth = (self._username, self._password)
            session.headers
            self._client = session
        return self._client
   
    def flush(self):
        self._cache = {}
        
    def _get(self, path):
        url = self.BASE_URL + path
        if url not in self._cache:
            response = self.client.get(url)
            if response.status_code == 200:
                self._cache[url] = response.json()
        return self._cache.get(url, {})
    
    @property
    def products(self):
        return self.get_products()
    
    def get_products(self):
        contents = self._get("mdr/products")
        specs = {}
        if contents:
            for aspect, asp_def in contents.get("_links").items():
                if aspect == "self":
                    continue
                for spec, spec_def in asp_def.get("_links").items():
                    if spec == "self":
                        continue
                    # Assumption
                    href = spec_def[0].get('href')
                    specs[spec] = href
                    
        return specs
    
    def adam(self, version="1-1"):
        """
        Get the ADaM Specifications
        """
        path = f"mdr/adam/adamig-{version}"
        response = self._get(path)
        if not response.status_code == 200:
            if response.status_code == 401:
                print("Authentication not recognised")
                return {}
            elif response.status_code == 404:
                print("Standard or Dataset not found")
                return {}
        return response.json()

    def adam_dataset(self, dataset, version="1-1"):
        """
        Get the ADaM Dataset Specifications
        """
        path = f"mdr/adam/adamig-{version}/{dataset}"
        response = self._get(path)
        if not response.status_code == 200:
            if response.status_code == 401:
                print("Authentication not recognised")
                return {}
            elif response.status_code == 404:
                print("Standard or Dataset not found")
                return {}
        return response.json()
    
    def adam_var(self, dataset, variable, version="1-1"):
        """
        Get the ADaM Dataset variable Specifications
        """
        path = f"mdr/adam/adamig-{version}/datastructures/{dataset}/variables/{variable}"
        response = self._get(path)
        if not response.status_code == 200:
            if response.status_code == 401:
                print("Authentication not recognised")
                return {}
            elif response.status_code == 404:
                print("Standard or Dataset not found")
                return {}
        return response.json()
        
    def sdtm(self, version="3-3"):
        """
        Get the SDTM Specifications
        """
        response = self._get(f"mdr/sdtmig/{version}")
        if not response.status_code == 200:
            if response.status_code == 401:
                print("Authentication not recognised")
                return {}
            elif response.status_code == 404:
                print("Standard or Dataset not found")
                return {}
        return response.json()
    
    def sdtm_dataset(self, dataset, version="3-3"):
        """
        Get the SDTM Dataset Specifications
        """
        response = self._get(f"mdr/sdtmig/{version}/datasets/{dataset}")
        if not response.status_code == 200:
            if response.status_code == 401:
                print("Authentication not recognised")
                return {}
            elif response.status_code == 404:
                print("Standard or Dataset not found")
                return {}
        return response.json()
    
    def sdtm_variable(self, dataset, variable, version="3-3"):
        """
        Get the SDTM Specifications
        """
        response = self._get(f"mdr/sdtmig/{version}/datasets/{dataset}/variables/{variable}")
        if not response.status_code == 200:
            if response.status_code == 401:
                print("Authentication not recognised")
                return {}
            elif response.status_code == 404:
                print("Standard or Dataset not found")
                return {}
        return response.json()
    
    def get_terminology_by_name(self, name, parent):
        """
        Given the username for the Codelist find the 
        """
        pass
    
        
    def terminology_set(self, name, parent="sdtm"):
        """
        Get the codelist
        """
        