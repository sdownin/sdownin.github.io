---
layout: archive
title: "Supplementary Code Excerpts and Explanations"
permalink: /code/
author_profile: true
---

{% include base_path %}

{% for post in site.code reversed %}
  {% include archive-single.html %}
{% endfor %}