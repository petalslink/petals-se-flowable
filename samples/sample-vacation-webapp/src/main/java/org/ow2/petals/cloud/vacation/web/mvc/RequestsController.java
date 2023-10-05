/**
 * Copyright (c) 2015-2023 Linagora
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

import org.ow2.petals.cloud.vacation.web.VacationRequest.PendingVacationRequest;
import org.ow2.petals.cloud.vacation.web.services.FlowableProcessClient;
import org.ow2.petals.cloud.vacation.web.services.FlowableTaskClient;
import org.ow2.petals.cloud.vacation.web.services.VacationClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import jakarta.validation.Valid;

/**
 * 
 * @author vnoel
 *
 */
@Controller
@RequestMapping("/")
public class RequestsController extends AbstractController {

    @Autowired
    private FlowableProcessClient flowableProcessClient;

    @Autowired
    private FlowableTaskClient flowableTaskClient;

    /**
     * Used in the templates
     */
    private static final String REQUEST_ATTRIBUTE = "request";

    /**
     * TODO
     * 
     * pending and archived requests are disabled for now because they implies to infer from the state of a process the
     * state of the request itself...
     */
    @RequestMapping
    public ModelAndView list() {
        final ModelAndView modelAndView = new ModelAndView("pages/requests");
        modelAndView.addObject("refusedRequests",
                flowableTaskClient.getRefusedRequests(userSession.getUsernameWithChecks()));

        modelAndView.addObject("pendingRequests",
                flowableProcessClient.getMyPendingRequests(userSession.getUsernameWithChecks()));
        modelAndView.addObject("archivedRequests",
                flowableProcessClient.getMyFinishedRequests(userSession.getUsernameWithChecks()));

        return modelAndView;
    }

    /**
     * cancel contains the id, which is converted automatically to a {@link PendingVacationRequest}.
     */
    @RequestMapping(method = RequestMethod.GET, params = "cancel")
    public ModelAndView cancel(final @RequestParam("cancel") PendingVacationRequest request,
            final RedirectAttributes redirect) {

        vacationClient.cancel(request);

        redirect.addFlashAttribute("globalMessage", "Successfully cancelled request " + request.getId());

        return new ModelAndView("redirect:/");
    }

    /**
     * edit contains the id, which is converted automatically to a {@link PendingVacationRequest}.
     */
    @RequestMapping(method = RequestMethod.GET, params = "edit")
    public ModelAndView editForm(final @RequestParam("edit") PendingVacationRequest request) {
        final ModelAndView modelAndView = new ModelAndView("pages/form");
        modelAndView.addObject(REQUEST_ATTRIBUTE, request);
        return modelAndView;
    }

    /**
     * edit contains the id, which is converted automatically to a {@link PendingVacationRequest}.
     * 
     * request is the new edited request, {@link VacationClient#edit(PendingVacationRequest, PendingVacationRequest)}
     * will populate it from the original one.
     */
    @RequestMapping(method = RequestMethod.POST, params = "edit")
    public ModelAndView edit(final @ModelAttribute(REQUEST_ATTRIBUTE) @Valid PendingVacationRequest request,
            final BindingResult result, final @RequestParam("edit") PendingVacationRequest original,
            final RedirectAttributes redirect) {

        if (result.hasErrors()) {
            return new ModelAndView("pages/form");
        }

        this.vacationClient.edit(original, request);

        redirect.addFlashAttribute("globalMessage", "Successfully edited request " + request.getId());

        return new ModelAndView("redirect:/");
    }

    /**
     * A new request is built and used as the basis for the form (it is needed for the default value for example)
     */
    @RequestMapping(method = RequestMethod.GET, params = "create")
    public String createForm(final @ModelAttribute(REQUEST_ATTRIBUTE) PendingVacationRequest request) {
        return "pages/form";
    }

    /**
     * request is the new created request
     */
    @RequestMapping(method = RequestMethod.POST, params = "create")
    public ModelAndView create(final @ModelAttribute(REQUEST_ATTRIBUTE) @Valid PendingVacationRequest request,
            final BindingResult result, final RedirectAttributes redirect) {

        if (result.hasErrors()) {
            return new ModelAndView("pages/form");
        }

        this.vacationClient.create(userSession.getUsernameWithChecks(), request);

        redirect.addFlashAttribute("globalMessage", "Successfully created a new request");

        return new ModelAndView("redirect:/");
    }
}
