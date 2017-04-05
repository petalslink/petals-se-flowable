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
package org.ow2.petals.flowable.identity.file;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.activiti.engine.ActivitiException;
import org.activiti.engine.identity.Group;
import org.activiti.engine.identity.Picture;
import org.activiti.engine.identity.User;
import org.activiti.engine.identity.UserQuery;
import org.activiti.engine.impl.Page;
import org.activiti.engine.impl.UserQueryImpl;
import org.activiti.engine.impl.context.Context;
import org.activiti.engine.impl.persistence.AbstractManager;
import org.activiti.engine.impl.persistence.entity.IdentityInfoEntity;
import org.activiti.engine.impl.persistence.entity.UserIdentityManager;

public class FileUserManager extends AbstractManager implements UserIdentityManager {

    /**
     * Users
     */
    final Map<String, User> users;

    /**
     * <p>
     * Groups by user.
     * </p>
     * <p>
     * The key is the user identifier, the value is the list of group identifier associated to the user/
     * </p>
     */
    final Map<String, List<Group>> groupsByUser;

    public FileUserManager(final Map<String, User> users, final Map<String, List<Group>> groupsByUser) {
        this.users = users;
        this.groupsByUser = groupsByUser;
    }

    @Override
    public User createNewUser(final String userId) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support creating a new user");
    }

    @Override
    public void insertUser(final User user) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support inserting a new user");
    }

    @Override
    public void updateUser(final User updatedUser) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support updating a user");
    }

    @Override
    public void deleteUser(final String userId) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support deleting a user");
    }

    @Override
    public User findUserById(final String userId) {
        return this.users.get(userId);
    }

    @Override
    public List<User> findUserByQueryCriteria(final UserQueryImpl query, final Page page) {

        final List<UserCriteria> criterias = new ArrayList<UserCriteria>();
        if (query.getGroupId() != null) {
            criterias.add(new UserGroupIdCriteria(this.groupsByUser, query.getGroupId()));
        }

        List<User> result = new ArrayList<User>(this.users.values());
        for (final UserCriteria criteria : criterias) {
            result = criteria.meetCriteria(result);
        }

        return result;
    }

    @Override
    public long findUserCountByQueryCriteria(final UserQueryImpl query) {
        return findUserByQueryCriteria(query, null).size();
    }

    @Override
    public List<Group> findGroupsByUser(final String userId) {
        return this.groupsByUser.get(userId);
    }

    @Override
    public UserQuery createNewUserQuery() {
        return new UserQueryImpl(Context.getProcessEngineConfiguration().getCommandExecutor());
    }

    @Override
    public IdentityInfoEntity findUserInfoByUserIdAndKey(final String userId, final String key) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support querying on user attributes");
    }

    @Override
    public List<String> findUserInfoKeysByUserIdAndType(final String userId, final String type) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support querying on user attributes");
    }

    @Override
    public Boolean checkPassword(final String userId, final String password) {

        final User user = this.users.get(userId);
        return Boolean.valueOf(user != null && password != null && password.equals(user.getPassword()));
    }

    @Override
    public List<User> findPotentialStarterUsers(final String proceDefId) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support querying on user attributes");
    }

    @Override
    public List<User> findUsersByNativeQuery(final Map<String, Object> parameterMap, final int firstResult,
            final int maxResults) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support native query");
    }

    @Override
    public long findUserCountByNativeQuery(final Map<String, Object> parameterMap) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support native query");
    }

    @Override
    public boolean isNewUser(final User user) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support checking if a user is a new user");
    }

    @Override
    public Picture getUserPicture(final String userId) {
        // The user manager of the identity service based on file doesn't support user picture
        return null;
    }

    @Override
    public void setUserPicture(final String userId, final Picture picture) {
        throw new ActivitiException(
                "The user manager of the identity service based on file doesn't support user picture");
    }

}
