/**
 * Copyright (c) 2015-2017 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETUSER;

import java.net.URI;
import java.util.List;
import java.util.logging.Logger;

import org.flowable.engine.IdentityService;
import org.flowable.idm.api.UserQuery;
import org.ow2.petals.components.flowable.generic._1.GetUser;
import org.ow2.petals.components.flowable.generic._1.GetUserResponse;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.components.flowable.generic._1.UnknownUser;
import org.ow2.petals.components.flowable.generic._1.User;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.flowable.incoming.integration.exception.UnknownUserException;

/**
 * The integration operation to search tasks
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetUserOperation extends AbstractOperation<GetUser, GetUserResponse> {

    public static final URI FAULT_ACTOR = URI.create("http://petals.ow2.org/components/flowable/generic/1.0/GetUser");

    /**
     * The Flowable identity service
     */
    private final IdentityService identityService;

    public GetUserOperation(final IdentityService identityService, final Logger log)
            throws OperationInitializationException {
        super(ITG_OP_GETUSER, FAULT_ACTOR,
                new Class[] { GetUser.class, GetUserResponse.class, InvalidRequest.class, UnknownUser.class }, log);
        this.identityService = identityService;
    }

    @Override
    public GetUserResponse doExecute(final GetUser incomingObject) throws Exception {

        final UserQuery userQuery = this.identityService.createUserQuery();
        userQuery.userId(incomingObject.getId());
        
        final List<org.flowable.idm.api.User> users = userQuery.list();
        if (users.isEmpty()) {
            throw new UnknownUserException(incomingObject.getId());
        } else {
            final org.flowable.idm.api.User foundUser = users.get(0);
            final User user = new User();
            user.setId(foundUser.getId());
            user.setFirstName(foundUser.getFirstName());
            user.setLastName(foundUser.getLastName());
            user.setEmail(foundUser.getEmail());
            final GetUserResponse response = new GetUserResponse();
            response.setUser(user);
            return response;
        }
    }

}
