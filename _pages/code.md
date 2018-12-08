---
layout: archive
title: "Code Excerpts"
permalink: /code/
author_profile: true
---

{% include base_path %}

{% for post in site.code reversed %}
  {% include archive-single.html %}
{% endfor %}
