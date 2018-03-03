import pandas as pd
import spotipy
import spotipy.util
import os
from IPython.display import clear_output

def get_auth_token(username, client_id, client_secret, redirect_uri):
    try:
        os.remove(f".cache-{username}")
    except:
        None
        
    token = spotipy.util.prompt_for_user_token(username,
                                               client_id=client_id,
                                               client_secret=client_secret,
                                               redirect_uri=redirect_uri)

    return token


def query_spotify(songs_df, token, verbose=False):
    """
    Function queries the spotifyAPI, by using the artist name and the song title
    :param filepath: str | filepath
    :return: pd.DataFrame | dataframe with the audio features from the spotifyAPI
    """
    # search for track + name
    AudioFeatures2 = pd.DataFrame()
    
    failed = []
    for index, row in songs_df.iterrows():
        name = row['song']
        artist = row['artist']
        year = row['year']
        try:
            spotify = spotipy.Spotify(auth=token)
            results = spotify.search(
                q='track:' + name + ' artist:' + artist, type='track',
                limit=1)
            uri = results['tracks']['items'][0]['uri']
            spotify_name = results['tracks']['items'][0]['name']
            album_name = results['tracks']['items'][0]['album']['name']
            sp_artist = results['tracks']['items'][0]['artists'][0]['name']
            popularity = results['tracks']['items'][0]['popularity']
            release_date = results['tracks']['items'][0]['album']['release_date']
            explicit = results['tracks']['items'][0]['explicit']
            
            if verbose:
                print('successfully queried song {}: {} by {}'.format(index, name, artist))

            audio_feat = spotify.audio_features(uri)
            x = audio_feat[0]
            x.pop('analysis_url')
            x.pop('track_href')
            x['spotify_name'] = spotify_name
            x['spotify_album_name'] = album_name
            x['spotify_artist'] = sp_artist
            x['billboard_name'] = name
            x['billboard_artist'] = artist
            x['popularity'] = popularity
            x['release_date'] = release_date
            x['explicit'] = explicit
            x['billboard_df_index'] = index
            
            AudioFeatures2 = AudioFeatures2.append(x, ignore_index=True)
        except:
            if verbose:
                print('failed song {}: {} by {} from {}'.format(index, name, artist, year))
            failed.append([name, artist, year])
            None
            
    failed = pd.DataFrame(failed, columns=['name', 'artist', 'year'])
    return AudioFeatures2, failed