def has_focus(self):
        return self == self.qtile.current_window

    def has_fixed_ratio(self):
        try:
            if ('PAspect' in self.hints['flags'] and
                    self.hints["min_aspect"] == self.hints["max_aspect"]):
                return True
        except KeyError:
            pass
        return False

    def has_fixed_size(self):
        try:
            if ('PMinSize' in self.hints['flags'] and
            self.hints['input'] = h['input']

        if getattr(self, 'group', None):
            if self.group.floating_layout.match(self):
                self.floating = True
            self.group.layout_all()

        return

             if "PMaxSize" in flags:
                width = min(width, self.hints.get('max_width', 0)) or width
                height = min(height, self.hints.get('max_height', 0)) or height
            if "PAspect" in flags:
                min_aspect = self.hints["min_aspect"]
                max_aspect = self.hints["max_aspect"]
                if width / height < min_aspect[0] / min_aspect[1]:
                    height = width * min_aspect[1] // min_aspect[0]
                elif width / height > max_aspect[0] / max_aspect[1]:
                    height = width * max_aspect[1] // max_aspect[0]

            if self.hints['base_width'] and self.hints['width_inc']:
                width_adjustment = (width - self.hints['base_width']) % self.hints['width_inc']
