/**
 * Copyright (c) 2015-2025 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.cloud.vacation.web.mvc;

import org.ow2.petals.cloud.vacation.web.UserSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;

/**
 * 
 * @author vnoel
 *
 */
@Controller
@RequestMapping("/login")
public class LoginController {

    /**
     * Used in the template
     */
    private static final String USER_ATTRIBUTE = "user";

    @Autowired
    private UserSession userSession;

    @RequestMapping(method = RequestMethod.GET)
    public ModelAndView login(final UserModel user) {
        return new ModelAndView("pages/login", USER_ATTRIBUTE, user);
    }

    @RequestMapping(method = RequestMethod.POST)
    public ModelAndView doLogin(final @ModelAttribute(USER_ATTRIBUTE) @Valid UserModel user, final BindingResult result,
            final RedirectAttributes redirect) {

        if (result.hasErrors()) {
            return new ModelAndView("pages/login");
        }

        userSession.setUsername(user.username);

        redirect.addFlashAttribute("globalMessage", "Successfully logged as " + user.username);

        return new ModelAndView("redirect:/");
    }

    public static class UserModel {

        @NotEmpty(message = "Username must not be empty")
        private String username;

        public String getUsername() {
            return username;
        }

        public void setUsername(String username) {
            this.username = username;
        }

    }
}
