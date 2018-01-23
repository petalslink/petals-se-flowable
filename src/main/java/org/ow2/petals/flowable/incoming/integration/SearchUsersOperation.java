/**
 * Copyright (c) 2017-2018 Linagora
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
package org.ow2.petals.flowable.incoming.integration;

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_SEARCHUSERS;

import java.net.URI;
import java.util.List;
import java.util.logging.Logger;

import org.flowable.engine.IdentityService;
import org.flowable.idm.api.UserQuery;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.components.flowable.generic._1.SearchUsers;
import org.ow2.petals.components.flowable.generic._1.SearchUsersResponse;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;

/**
 * The integration operation to search users
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class SearchUsersOperation extends AbstractOperation<SearchUsers, SearchUsersResponse> {

    public static final URI FAULT_ACTOR = URI
            .create("http://petals.ow2.org/components/flowable/generic/1.0/SearchUsers");

    /**
     * The Flowable identity service
     */
    private final IdentityService identityService;

    public SearchUsersOperation(final IdentityService identityService, final Logger log)
            throws OperationInitializationException {
        super(ITG_OP_SEARCHUSERS, FAULT_ACTOR,
                new Class[] { SearchUsers.class, SearchUsersResponse.class, InvalidRequest.class }, log);
        this.identityService = identityService;
    }

    @Override
    public SearchUsersResponse doExecute(final SearchUsers incomingObject) throws Exception {

        final UserQuery userQuery = this.identityService.createUserQuery();
        userQuery.memberOfGroup(incomingObject.getGroupId());

        final List<org.flowable.idm.api.User> users = userQuery.list();
        final SearchUsersResponse response = new SearchUsersResponse();
        for (final org.flowable.idm.api.User foundUser : users) {
            response.getUserId().add(foundUser.getId());
        }
        return response;
    }

}
