/**
 * Copyright (c) 2017 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_SEARCHGROUPS;

import java.net.URI;
import java.util.List;
import java.util.logging.Logger;

import org.flowable.engine.IdentityService;
import org.flowable.idm.api.Group;
import org.flowable.idm.api.GroupQuery;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.components.flowable.generic._1.SearchGroups;
import org.ow2.petals.components.flowable.generic._1.SearchGroupsResponse;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;

/**
 * The integration operation to search users
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class SearchGroupsOperation extends AbstractOperation<SearchGroups, SearchGroupsResponse> {

    public static final URI FAULT_ACTOR = URI
            .create("http://petals.ow2.org/components/flowable/generic/1.0/SearchGroups");

    /**
     * The Flowable identity service
     */
    private final IdentityService identityService;

    public SearchGroupsOperation(final IdentityService identityService, final Logger log)
            throws OperationInitializationException {
        super(ITG_OP_SEARCHGROUPS, FAULT_ACTOR,
                new Class[] { SearchGroups.class, SearchGroupsResponse.class, InvalidRequest.class }, log);
        this.identityService = identityService;
    }

    @Override
    public SearchGroupsResponse doExecute(final SearchGroups incomingObject) throws Exception {

        final GroupQuery groupQuery = this.identityService.createGroupQuery();
        groupQuery.groupMember(incomingObject.getUserId());

        final List<Group> groups = groupQuery.list();
        final SearchGroupsResponse response = new SearchGroupsResponse();
        for (final org.flowable.idm.api.Group foundGroup : groups) {
            response.getGroupId().add(foundGroup.getId());
        }
        return response;
    }

}
