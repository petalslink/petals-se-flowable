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
package org.ow2.petals.activitibpmn.identity.file;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.activiti.engine.ActivitiException;
import org.activiti.engine.ActivitiIllegalArgumentException;
import org.activiti.engine.identity.Group;
import org.activiti.engine.identity.GroupQuery;
import org.activiti.engine.impl.GroupQueryImpl;
import org.activiti.engine.impl.Page;
import org.activiti.engine.impl.context.Context;
import org.activiti.engine.impl.persistence.AbstractManager;
import org.activiti.engine.impl.persistence.entity.GroupIdentityManager;

import com.ebmwebsourcing.easycommons.lang.StringHelper;

public class FileGroupManager extends AbstractManager implements GroupIdentityManager {
    
    /**
     * <p>
     * Groups by user.
     * </p>
     * <p>
     * The key is the user identifier, the value is the list of group identifier associated to the user/
     * </p>
     */
    final Map<String, List<Group>> groupsByUser;
    
    public FileGroupManager(Map<String, List<Group>> groupsByUser) {
        this.groupsByUser = groupsByUser;
    }

    @Override
    public Group createNewGroup(final String groupId) {
        throw new ActivitiException(
                "The group manager of the identity service based on file doesn't support creating a new group");
    }

    @Override
    public void insertGroup(final Group group) {
        throw new ActivitiException(
                "The group manager of the identity service based on file doesn't support inserting a new group");
    }

    @Override
    public void updateGroup(final Group updatedGroup) {
        throw new ActivitiException(
                "The group manager of the identity service based on file doesn't support updating a group");
    }

    @Override
    public void deleteGroup(final String groupId) {
        throw new ActivitiException(
                "The group manager of the identity service based on file doesn't support deleting a group");
    }

    @Override
    public GroupQuery createNewGroupQuery() {
        return new GroupQueryImpl(Context.getProcessEngineConfiguration().getCommandExecutor());
    }

    @Override
    public List<Group> findGroupByQueryCriteria(final GroupQueryImpl query, final Page page) {
        // Only support for groupMember() at the moment
        if (!StringHelper.isNullOrEmpty(query.getUserId())) {
          return this.findGroupsByUser(query.getUserId());
        } else {
          throw new ActivitiIllegalArgumentException("This query is not supported by the LDAPGroupManager");
        }
    }

    @Override
    public long findGroupCountByQueryCriteria(final GroupQueryImpl query) {
        return this.findGroupByQueryCriteria(query, null).size();
    }

    @Override
    public List<Group> findGroupsByUser(final String userId) {
        final List<Group> groups = this.groupsByUser.get(userId);
        if (groups == null) {
            // Unknown user
            return new ArrayList<Group>(0);
        } else {
            return this.groupsByUser.get(userId);
        }
    }

    @Override
    public List<Group> findGroupsByNativeQuery(final Map<String, Object> parameterMap, final int firstResult, final int maxResults) {
        throw new ActivitiException("The group manager of the identity service based on file doesn't support native querying");
    }

    @Override
    public long findGroupCountByNativeQuery(final Map<String, Object> parameterMap) {
        throw new ActivitiException("The group manager of the identity service based on file doesn't support native querying");
    }

    @Override
    public boolean isNewGroup(final Group group) {
        throw new ActivitiException(
                "The group manager of the identity service based on file doesn't support checking if a group is a new group");
    }

}
