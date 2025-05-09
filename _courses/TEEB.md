---
layout: page
title:  "Theoretical Ecology and Evolutionary Biology - Topic Group"
permalink: /TEEB
---

<div class="jumbotron jumbotron-fluid mb-3 pl-0 pt-0 pb-0 bg-white position-relative">
    <div class="h-100 tofront">
        <div class="row justify-content-between">
            <div class="col-md-6 pr-0 pr-md-4 pt-4 pb-4 align-self-center">
                <div class="page-content" style="text-align:justify">
                    The topic group takes place every other Friday from <b>10:00 to 11:30 in room 1929</b>, on the first floor of the Biophore. During our sessions, we discuss a theoretical question by either going through a paper or listening to someone talk about their own work (or a mixture of the two). Each session is hosted by a different member of the topic group. The host is free to choose the topic of the day, the only requirement being that it must be theory in ecology and evolutionary biology. Our schedule is given below along with guidelines on how to host a session. Discussed papers are uploaded on this page a week before the session, it is important to read them before coming. 
                </div>
            </div>
            <div class="col-md-6 pr-0 align-self-center">
                <img class="rounded" src="/assets/images/topic-group-picture.jpeg" alt="Topic group">
            </div>
        </div>
    </div>
</div>

Each new session is announced by e-mail. To join our mailing list, please contact Thomas Lesaffre ([*thomas.lesaffre@unil.ch*](mailto:thomas.lesaffre@unil.ch)). 


<h3 class="font-weight-bold spanborder"><span>Next sessions </span></h3>

<i class="fa fa-clock-o fa-lg" aria-hidden="true"></i> <b>NEW SCHEDULE FROM MARCH 14TH:</b> Next sessions from 10:00 to 11:30. 

<div class="row gap-y listrecent listrecent listauthor">
    {% for session in site.data.teeb_sessions %}
        {% if session.next %}
            {% include session-template.html alt="120px" grey="false" %}
        {% endif %}
    {% endfor %}
</div>


<h3 class="font-weight-bold spanborder"><span>Guidelines for hosting a topic group session</span></h3>


#### Before the session

* You are free to choose the topic of the day, as long as it fits the scope of our topic group (i.e. __theory__ in ecology or evolutionary biology). You may decide to present some of your work, a published paper (which can be classical or recent), or a mixture of the two. However, __after the session preceding yours at the latest, you must talk to Sara, Charles and/or Laurent about what you plan to present__ so that they validate it or suggest alternatives.

* __At least a week before your session__, send a message to the organiser ([*thomas.lesaffre@unil.ch*](mailto:thomas.lesaffre@unil.ch)) briefly explaining your plan for the session (one or two sentences is enough) with the paper you wish to discuss attached, if any.

* You are __responsible for the session that was assigned to you__. If you cannot host it, you need to e-mail the organiser well in advance to give time for someone to step in.

#### During the session

* Start your presentation with a short introduction. Explain what you wish to accomplish by the end of the session, introduce the biological problem we are going to discuss, explain why it is interesting and how it fits in the literature.

* Give a complete biological description of the model, which should include information on:

<ol>
<ol type="i">
<li>the general assumptions: what is the demography of the population (e.g. constant or fluctuating size, class-structure, is the population well-mixed or structured)? 
What are the features by which individuals are characterised (e.g. are sexes combined or separate, what are the traits relevant to characterise them in the model)?</li> 
<li> the life cycle: what are the events occurring over the course of a timestep? Be thorough for each of them, and introduce relevant parameters and functions.</li>
<li> the evolving trait(s) and their underlying genetic architecture (e.g. ploidy, number of loci are involved, mutation model...).</li>
</ol>
</ol>

* Outline the method that we will use to attack the model and explain what we are going to ask our model (e.g. what are we solving for?). Importantly, the __method does not have to be the one presented in the paper__. If you think the same results can be obtained with a more modern approach for example, go for it!

* Explain the logic of the analysis and how the results are obtained without going into the details of the calculations unless you think it is important to do so. In any case, __you should avoid doing long derivations on the board__.  

* __Use the screen__ to show plots, important equations etc... If you make slides, they have to be clear but they do not need to be pretty (no one cares!). A Mathematica notebook can do the trick.



<h3 class="font-weight-bold spanborder"><span>Past sessions</span></h3>

<div class="row gap-y listrecent listrecent listauthor">
    {% for session in site.data.teeb_sessions reversed %}
        {% unless session.next %}
            {% include session-template.html alt="160px" grey="true" %}
        {% endunless %}
    {% endfor %}
</div>


