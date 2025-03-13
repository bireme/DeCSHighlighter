import requests
import sys
from typing import List, Dict

BASE_URL = "http://localhost:9090"

def fetch_scores(text: str) -> List[Dict[str, int | float]]:
    """
    Envia uma requisição POST ao serviço e retorna uma lista de elementos
    contendo os campos "descriptor", "quantity" e "score".

    :param text: O texto que será passado como parâmetro.
    :return: Lista de dicionários com os campos "descriptor", "quantity" e "score".
    """
    url = f"{BASE_URL}/decshighlighter/serv"
    print(url)
    payload = {
        "showText": "f",
        "showPositions": "f",
        "showDescriptors": "f",
        "document": text
    }

    try:
        response = requests.post(url, json=payload)
        response.raise_for_status()
        data = response.json()

        return [
            {"descriptor": item["descriptor"], "quantity": int(item["quantity"]), "score": float(item["score"])}
            for item in data.get("scores", [])
        ]
    except requests.exceptions.RequestException as e:
        print(f"Erro ao acessar o serviço: {e}")
        return []

def get_text_input(text_arg: str) -> str:
    """
    Verifica se o argumento começa com "file=" e lê o conteúdo do arquivo correspondente.
    Caso contrário, retorna o próprio argumento.
    """
    if text_arg.startswith("file="):
        file_path = text_arg[5:]
        try:
            with open(file_path, "r", encoding="utf-8") as file:
                return file.read().strip()
        except FileNotFoundError:
            print(f"Erro: Arquivo '{file_path}' não encontrado.")
            sys.exit(1)
        except IOError as e:
            print(f"Erro ao ler o arquivo '{file_path}': {e}")
            sys.exit(1)
    return text_arg

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Uso: python fetchScores.py <texto> ou python fetchScores.py file=<caminho_do_arquivo>")
        sys.exit(1)

    text_input = get_text_input(sys.argv[1])
    result = fetch_scores(text_input)
    print(result)
