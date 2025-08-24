% Projet IA Enquête policiere en utilisant SWI-Prolog
% Serveur HTTP avec formulaire et verdict
% ============================================
:- module(enquete, [server/0, server/1, stop/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_error)).

% -----------------------------------
% Supprimer warnings de faits disperse
% -----------------------------------
:- discontiguous has_motive/2.
:- discontiguous was_near_crime_scene/2.
:- discontiguous has_fingerprint_on_weapon/2.
:- discontiguous has_bank_transaction/2.
:- discontiguous owns_fake_identity/2.
:- discontiguous eyewitness_identification/2.

% ------------------------------------
% Types de crime
% ------------------------------------
crime_type(vol).
crime_type(assassinat).
crime_type(escroquerie).

% -----------------------------------
% Suspects
% ----------------------------------
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

% ---------------------------------
% Faits : indices et mobiles
% ----------------------------------
% --- VOL ---
has_motive(john, vol).
was_near_crime_scene(john, vol).
has_fingerprint_on_weapon(john, vol).

% --- ASSASSINAT ---
has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).

% --- ESCROQUERIE ---
has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).
has_bank_transaction(bruno, escroquerie).
owns_fake_identity(sophie, escroquerie).

% --- Temoignage ---
eyewitness_identification(mary, assassinat).

% ---------------------------------------
% Regles de culpabilite
% ---------------------------------------
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

% -------------------------------------
% Serveur HTTP & Routes
% --------------------------------------
:- http_handler(root(.), page_home, []).
:- http_handler(root(verdict), page_verdict, []).

server        :- server(8002).
server(Port) :-
    http_server(http_dispatch, [port(Port), ip(localhost)]),
    format('Serveur lance sur http://localhost:~w~n', [Port]).

stop :-
    http_current_server(Port, _),
    http_stop_server(Port, []),
    format('Serveur arrête (port ~w).~n', [Port]).

% ------------------------------------
% Pages
% ------------------------------------
page_home(_Request) :-
    all_suspects(Suspects),
    all_crimes(Crimes),
    reply_html_page(
        page_style('Accueil'),
        \page_header('Systeme expert Enquete policiere'),
        \form_block(Suspects, Crimes)
    ).

page_verdict(Request) :-
    catch(
        http_parameters(Request, [
            suspect(SuspectAtom, [atom]),
            crime(CrimeAtom, [atom])
        ]),
        _E,
        ( reply_html_page(page_style('Erreur'),
                          \page_header('Parametres invalides'),
                          p('Parametres incorrects')), !
        )
    ),
    ( \+ suspect(SuspectAtom) ->
        reply_html_page(page_style('Erreur'),
                        \page_header('Erreur'),
                        p(['Le suspect "', b(SuspectAtom), '" est inconnu.'])), !
    ; \+ crime_type(CrimeAtom) ->
        reply_html_page(page_style('Erreur'),
                        \page_header('Erreur'),
                        p(['Le type de crime "', b(CrimeAtom), '" est inconnu.'])), !
    ; explain(SuspectAtom, CrimeAtom, Verdict, Proofs),
      reply_html_page(
          page_style('Verdict'),    
          \page_header('Analyse terminee'),
          \verdict_block(SuspectAtom, CrimeAtom, Verdict, Proofs)
      )
    ).

% ------------------------------------
% Vues (HTML avec CSS inline et flex)
% ------------------------------------
page_style(Title) -->
    html([
        head([
            title(Title),
            meta([charset('UTF-8')]),
            meta([name(viewport), content('width=device-width, initial-scale=1')])
        ])
    ]).

page_header(Title) -->
    html(div([style('display:flex; flex-direction:column; justify-content:center; align-items:center; background-color:blue; padding:20px; width:50%; margin:auto;')],
      [ h1([style('color:white; margin:0;')], Title),
        p([style('color:white; margin-top:10px;')], 'Choisissez un suspect et un type de crime, puis lancez l''analyse.')
      ])).

form_block(Suspects, Crimes) -->
    html(div([style('display:flex; justify-content:center; margin-top:30px;')],
      form([action='/verdict', method='POST', style('display:flex; flex-direction:column; gap:15px; width:300px;')],
        [ div([style('display:flex; flex-direction:column;')],
            [ label([for(suspect), style('margin-bottom:5px;')], 'Suspect:'),
              select([name=suspect, id=suspect, style('padding:10px; border-radius:8px;')], \options_from_atoms(Suspects))
            ]),
          div([style('display:flex; flex-direction:column;')],
            [ label([for(crime), style('margin-bottom:5px;')], 'Type de crime:'),
              select([name=crime, id=crime, style('padding:10px; border-radius:8px;')], \options_from_atoms(Crimes))
            ]),
          input([type(submit), value('Analyser'), style('padding:12px; border:none; border-radius:8px; background-color:blue; color:white; cursor:pointer;')])
        ]))).

verdict_block(Suspect, Crime, Verdict, Proofs) -->
    {
      ( Verdict = guilty -> BadgeColor = 'background-color:red; color:white; padding:6px 12px; border-radius:999px;', BadgeText = 'Coupable'
      ; BadgeColor = 'background-color:green; color:white; padding:6px 12px; border-radius:999px;', BadgeText = 'Non coupable'
      )
    },
    html(div([style('display:flex; justify-content:center; margin-top:30px;')],
      div([style('background-color:#f9f9f9; padding:24px; border-radius:16px; width:400px; box-shadow:0 8px 24px rgba(0,0,0,.15);')],
        [ h2([style('display:flex; justify-content:space-between; align-items:center; font-size:20px; font-weight:700; margin:0;')],
             ['Verdict : ', span([style(BadgeColor)], BadgeText)]),
          p([style('color:blue; margin:10px 0 0 0;')], ['Suspect : ', b(Suspect)]),
          p([style('color:blue; margin:5px 0 0 0;')], ['Crime : ',  b(Crime)]),
          \proofs_list(Proofs),
          a([href='/', style('margin-top:15px; display:inline-block; color:white; background-color:blue; padding:8px 12px; text-decoration:none; border-radius:8px;')], 'Retour a l''accueil')
        ])
    )).

proofs_list([]) --> html(p([style('color:gray; font-size:14px;')], 'Aucune preuve determinante.')).
proofs_list(Proofs) -->
    html([ h3([style('margin:15px 0 5px 0;')], 'elements pris en compte :'),
           ul([style('padding-left:20px;')],
               \proof_items(Proofs))
         ]).

proof_items([]) --> [].
proof_items([P|Ps]) --> html(li([style('margin-bottom:3px;')], P)), proof_items(Ps).

options_from_atoms([]) --> [].
options_from_atoms([A|As]) --> html(option([value(A)], A)), options_from_atoms(As).

% ---------------------------------------
% Donnees utilitaires
% ---------------------------------------
all_suspects(Ss) :- findall(S, suspect(S), Raw), sort(Raw, Ss).
all_crimes(Cs)   :- findall(C, crime_type(C), Raw), sort(Raw, Cs).

% -------------------------------------
% Explication / justification
% --------------------------------------
explain(S, C, guilty, Proofs) :-
    is_guilty(S, C),
    findall(Text, proof_text(S, C, Text), Proofs0),
    sort(Proofs0, Proofs), !.
explain(_S, _C, not_guilty, []) :- !.

proof_text(S, C, 'Mobile etabli')                :- has_motive(S, C).
proof_text(S, C, 'Presence proche de la scene du crime'):- was_near_crime_scene(S, C).
proof_text(S, C, 'Empreinte sur l''arme')        :- has_fingerprint_on_weapon(S, C).
proof_text(S, C, 'Transaction bancaire suspecte')   :- has_bank_transaction(S, C).
proof_text(S, C, 'Fausse identite detectee')       :- owns_fake_identity(S, C).
proof_text(S, C, 'Temoin oculaire')            :- eyewitness_identification(S, C).
