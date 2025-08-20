% =========================================
% Projet IA – Enquête policiere (SWI-Prolog)
% Serveur HTTP avec formulaire
% =========================================

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).

% -------------------------------
% Supprimer warnings de faits disperses
% -------------------------------
:- discontiguous has_motive/2.
:- discontiguous was_near_crime_scene/2.
:- discontiguous has_fingerprint_on_weapon/2.
:- discontiguous has_bank_transaction/2.
:- discontiguous owns_fake_identity/2.
:- discontiguous eyewitness_identification/2.

% -------------------------------
% Serveur HTTP
% -------------------------------
:- http_handler(root(.), accueil, []).
:- http_handler(root(verdict), verdict, []).

server :- server(8002).
server(Port) :-
    format('Serveur HTTP demarre sur http://localhost:~w/~n', [Port]),
    http_server(http_dispatch, [port(Port)]).

stop :- http_stop_server(_, []).

% -------------------------------
% Types de crime
% -------------------------------
crime_type(vol).
crime_type(assassinat).
crime_type(escroquerie).

% -------------------------------
% Suspects
% -------------------------------
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

% -------------------------------
% Faits : indices et mobiles
% -------------------------------
% has_motive
has_motive(john, vol).
has_motive(mary, assassinat).
has_motive(alice, escroquerie).

% was_near_crime_scene
was_near_crime_scene(john, vol).
was_near_crime_scene(mary, assassinat).

% has_fingerprint_on_weapon
has_fingerprint_on_weapon(john, vol).
has_fingerprint_on_weapon(mary, assassinat).

% has_bank_transaction
has_bank_transaction(alice, escroquerie).
has_bank_transaction(bruno, escroquerie).

% owns_fake_identity
owns_fake_identity(sophie, escroquerie).

% eyewitness_identification
eyewitness_identification(mary, assassinat).

% -------------------------------
% Regles de culpabilite
% -------------------------------
is_guilty(S, vol) :-
    has_motive(S, vol),
    was_near_crime_scene(S, vol),
    has_fingerprint_on_weapon(S, vol).

is_guilty(S, assassinat) :-
    has_motive(S, assassinat),
    was_near_crime_scene(S, assassinat),
    ( has_fingerprint_on_weapon(S, assassinat)
    ; eyewitness_identification(S, assassinat)
    ).

is_guilty(S, escroquerie) :-
    has_motive(S, escroquerie),
    ( has_bank_transaction(S, escroquerie)
    ; owns_fake_identity(S, escroquerie)
    ).

% -------------------------------
% Pages HTTP
% -------------------------------
accueil(_Request) :-
    findall(S, suspect(S), Suspects),
    findall(C, crime_type(C), Crimes),
    reply_html_page(
        title('Systeme expert – Enquête policiere'),
        [ h1('Bienvenue sur le serveur Prolog'),
          form([action='/verdict', method='POST'],
            [ p([ 'Selectionnez un suspect: ',
                  select([name=suspect], \options_from_atoms(Suspects))
                ]),
              p([ 'Selectionnez un type de crime: ',
                  select([name=crime], \options_from_atoms(Crimes))
                ]),
              p(input([type=submit, value='Analyser']))
            ])
        ]
    ).

verdict(Request) :-
    http_parameters(Request, [
        suspect(Suspect, [atom]),
        crime(Crime, [atom])
    ]),
    ( \+ suspect(Suspect) ->
        Verdict = ['Erreur : suspect inconnu']
    ; \+ crime_type(Crime) ->
        Verdict = ['Erreur : type de crime inconnu']
    ; ( is_guilty(Suspect, Crime) ->
          Verdict = ['Coupable']
      ; Verdict = ['Non coupable']
      )
    ),
    reply_html_page(
        title('Verdict'),
        [ h1('Resultat de l\'analyse'),
          p(['Suspect : ', b(Suspect)]),
          p(['Crime : ', b(Crime)]),
          p(['Verdict : ', b(Verdict)]),
          p(a([href='/'], 'Retour a l\'accueil'))
        ]
    ).

% -------------------------------
% Utilitaires
% -------------------------------
options_from_atoms([]) --> [].
options_from_atoms([H|T]) --> html([option([value=H], H)]), options_from_atoms(T).
